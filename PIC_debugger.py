'''
File name: PIC_debugger.py
Author: Michal Adamkiewicz
Date: 2014

A simple (doesn't implement all registers etc.) PIC debugger. Made it to help with my programing

usage:
run_PIC(reg,i)

where reg is iterable containing the register names as stings and i is a multiline string of assembly
Typing "help" will give a list of possible commands
'''

import sys
import traceback


class filex:

    def __init__(self,PIC):
        self.PIC=PIC
        self.byte=0

    def add(self,value):
        calc=self.byte+value.byte

        if(calc==0):
            self.PIC.files['status'].BSF('z')
        else:
            self.PIC.files['status'].BCF('z')

        if(calc>255):
            self.PIC.files['status'].BSF('c')
            calc= calc % 256
        else:
            self.PIC.files['status'].BCF('c')

        return calc
    def xor(self,value):
        calc=self.byte^value.byte

        if(calc==0):
            self.PIC.files['status'].BSF('z')
        else:
            self.PIC.files['status'].BCF('z')

        return calc

    def AND(self,value):
        calc=self.byte & value.byte

        if(calc==0):
            self.PIC.files['status'].BSF('z')
        else:
            self.PIC.files['status'].BCF('z')


        return calc
    def ior(self,value):
        calc=self.byte | value.byte

        if(calc==0):
            self.PIC.files['status'].BSF('z')
        else:
            self.PIC.files['status'].BCF('z')


        return calc
    def sub(self,value):
        calc=self.byte - value.byte

        if(calc==0):
            self.PIC.files['status'].BSF('z')
        else:
            self.PIC.files['status'].BCF('z')

        if(calc<0):
            self.PIC.files['status'].BCF('c')
            calc= calc % 256
        else:
            self.PIC.files['status'].BSF('c')

        return calc


    def BSF(self,position):
        self.byte=self.byte | (2**int(position))
    def BCF(self,position):
        self.byte=self.byte & ((2**8-1)-(2**int(position)))
    def RLF(self):
        calc = self.byte*2

        if(self.PIC.files['status'].test('c')):
            calc+=1

        if(calc>255):
            self.PIC.files['status'].BSF('c')
            calc= calc % 256
        else:
            self.PIC.files['status'].BCF('c')
        return calc
    def RRF(self):
        calc = self.byte/2

        if(self.PIC.files['status'].test('c')):
            calc+=128

        if(calc==int(calc)):
            self.PIC.files['status'].BCF('c')
        else:
            self.PIC.files['status'].BSF('c')
        calc=int(calc)
        return calc
    def print_decimal(self):
        print(format(self.byte,'d'))
    def print_binary(self):
        print('0'*(8-len(format(self.byte,'b')))+format(self.byte,'b'))
    def print_hex(self):
        print('0x'+format(self.byte,'x'))   
    def assign(self,val):
        self.byte=val
    def get(self):
        return self.byte
    def test(self, position):
        return (self.byte & 2**int(position))!=0
    def inc(self):
        calc=(self.byte+1)%256
        if(calc==0):
            self.PIC.files['status'].BSF('z')
        else:  
            self.PIC.files['status'].BCF('z')
        return calc
    def dec(self):
        calc=(self.byte-1)%256
        if(calc==0):
            self.PIC.files['status'].BSF('z')
        else:  
            self.PIC.files['status'].BCF('z')
        return calc
    def __repr__(self):
        out=format(self.byte,'b')
        return '0x'+format(self.byte,'x').upper()+" "+'0'*(8-len(out))+out+' '+format(self.byte,'d')

class status_filex(filex):
    def test(self, position):
        try:
            position={'c':0,'z':2,'dc':1}[position]
        except KeyError:
            pass
        return (self.byte & 2**position)!=0
    def BSF(self,position):
        try:
            position={'c':0,'z':2}[position]
        except KeyError:
            pass
        self.byte=self.byte | (2**position)
    def BCF(self,position):
        try:
            position={'c':0,'z':2}[position]
        except KeyError:
            pass
        self.byte=self.byte & ((2**8-1)-(2**position))

class pointer_filex(filex):
    def add(self,value):
        calc=self.byte+value.byte

        if(calc==0):
            self.PIC.files['status'].BSF('z')
        else:
            self.PIC.files['status'].BCF('z')
        return calc

class PIC:
    def __init__(self,files_definitions,instruct):

        self.comand_list={'goto':self.goto,
                            'call':self.call,
                            'return':self.retur,
                            'bsf':self.bsf,
                            'bcf':self.bcf,
                            'clrf':self.clrf,
                            'clrw':self.clrw,
                            'btfss':self.btfss,
                            'btfsc':self.btfsc,
                            'rrf':self.rrf,
                            'rlf':self.rlf,
                            'movf':self.movf,
                            'movwf':self.movwf,
                            'movfw':self.movfw,
                            'movlw':self.movlw,
                            'parse_literal':self.parse_literal,
                            'nop':self.nop,
                            'retlw':self.retlw,
                            'incf':self.incf,
                            'decf':self.decf,
                            'decfsz':self.decfsz,
                            'incfsz':self.incfsz,
                            'addwf':self.addwf,
                            'andwf':self.andwf,
                            'xorwf':self.xorwf,
                            'iorwf':self.iorwf,
                            'subwf':self.subwf,
                            'addlw':self.addlw,
                            'andlw':self.andlw,
                            'xorlw':self.xorlw,
                            'iorlw':self.iorlw,
                            'sublw':self.sublw,
                            'end':self.END,
                            'p':self.ptr,
                            'setbp':self.set_breakpoint,
                            'clrbp':self.clear_breakpoint,
                            'n':self.run_till_breakpoint,
                            'rc':self.reset_counter,
                            'pc':self.print_counter,
                            'help':self.help
                            }

        self.call_stack=[]
        self.breakpoints=[]
        self.bp_mode=0
        self.files={}
        self.files['status']=status_filex(self)
        self.files['w']=filex(self)
        self.files['pcl']=pointer_filex(self)
        for f in files_definitions:
            self.files[f]=filex(self)

        self.label_lookup={}
        self.instructions=[]
        self.comments=[]
        pointer_temp=0

        coment=''
        for line in filter(lambda x:x!='',instruct):
            try:
                coment+=' ;'+line.split(';')[1]
            except IndexError:
                pass
            line=line.split(';')[0].lower()# removes comments



            lab=False
            try:
                if(line[0]!=' '):
                    lab=True
            except IndexError:
                pass
            if lab:
                comand=line.split(maxsplit=2)# removes extra whitespace
            else:
                comand=line.split(maxsplit=1)
            
            if lab:
                try:
                    self.label_lookup[comand[0]]
                    print('Label can be defined in only one place!', comand[0])
                except KeyError:
                    self.label_lookup[comand[0]]=pointer_temp
                comand.pop(0)                

            try:
                comand[0]=comand[0].lower()
                self.instructions.append(list(map(lambda x:x.strip(),comand)))
                self.comments.append(coment)
                pointer_temp+=1
                coment=''
            except IndexError:
                pass

        for x,t in enumerate(self.instructions):
            print(x,t)

        for x,t in self.label_lookup.items():
            print(x,t)


        self.inverse_label={v: k for k, v in self.label_lookup.items()}

        self.cycle_counter=0

    def help(self):
        print('''
    Welcome to the interactive PIC assembly code debugger
            
    To step through your code one instruction at time just press return,
    executed instruction will appear on screen

    Commands can be manually executed at the current location
    by typing them in. In addition to standard opcodes the following 
    ones are available to help with control of the debugger:

    p X - prints the value of register X
    setbp X - sets label X to act as breakpoint 
    clrbp X - stops label X from acting as breakpoint
    n - runs program until breakpoint is encountered

    rc - resets the cycle counter to zero
    pc - prints the cycle counter

    help - Displays this help text

            ''')

    def reset_counter(self):
        self.cycle_counter=0

    def print_counter(self):
        print(self.cycle_counter)

    def set_breakpoint(self,lable):
        self.breakpoints.append(self.label_lookup[lable])

    def clear_breakpoint(self,lable):
        self.breakpoints.pop(self.breakpoints.index(self.label_lookup[lable]))

    def run_till_breakpoint(self):
        self.bp_mode=1

    def manual_exectue(self,comand):
        curent=list(map(lambda x: x.strip(),comand.split(maxsplit=1)))
        try:
            self.comand_list[curent[0]](curent[1])
        except IndexError:
            self.comand_list[curent[0]]()

        if(curent[0]=='goto' or curent=='call' or curent=='return'):
            self.files['pcl'].assign(self.files['pcl'].get()+1)

    def ptr(self,arg):
        print(self.files[arg])

    def execute(self):
        curent=self.instructions[self.files['pcl'].get()]

        try:
            terminate=self.comand_list[curent[0]](curent[1])
        except IndexError:
            terminate=self.comand_list[curent[0]]()

        if(len(self.call_stack)>7): print('ERROR: CALL STACK OVERFLOW')
        self.files['pcl'].assign(self.files['pcl'].get()+1)

        try:
            print(self.inverse_label[self.files['pcl'].get()-1])
        except KeyError:
            pass
        print(self.files['pcl'].get(),curent)
        print(self.comments[self.files['pcl'].get()])

        if(self.files['pcl'].get() not in self.breakpoints and self.bp_mode==1):
            return 2

        self.bp_mode=0
        if(terminate):
            return 0
        else:
            return 1

    def parse_literal(self,lit):
        if(lit[0]=='b'): return int(lit[2:-1],2)%256
        if(lit[0]=='d'): return int(lit[2:-1],10)%256
        if(lit[0:2]=='0x'): return int(lit[2:],16)%256


    def goto(self,label):
        self.files['pcl'].assign(self.label_lookup[label]-1)
        self.cycle_counter+=2

    def call(self,label):
        self.call_stack.append(self.files['pcl'].get()+1)
        self.files['pcl'].assign(self.label_lookup[label]-1)
        self.cycle_counter+=2

    def retur(self):
        self.files['pcl'].assign(self.call_stack.pop()-1)
        self.cycle_counter+=2

    def bsf(self,arg):
        arg=list(map(lambda x: x.strip(),arg.split(',')))
        self.files[arg[0]].BSF(arg[1])
        self.cycle_counter+=1

    def bcf(self,arg):
        arg=list(map(lambda x: x.strip(),arg.split(',')))
        self.files[arg[0]].BCF(arg[1])
        self.cycle_counter+=1

    def clrf(self,fil):
        self.files[fil].assign(0)
        self.files['status'].BSF('z')
        self.cycle_counter+=1

    def clrw(self):
        self.files['w'].assign(0)
        self.files['status'].BSF('z')
        self.cycle_counter+=1

    def btfss(self,arg):
        self.cycle_counter+=1
        arg=list(map(lambda x: x.strip(),arg.split(',')))
        if self.files[arg[0]].test(arg[1]):
            self.files['pcl'].assign(self.files['pcl'].get()+1)
            self.cycle_counter+=1

    def btfsc(self,arg):
        self.cycle_counter+=1
        arg=list(map(lambda x: x.strip(),arg.split(',')))
        if not self.files[arg[0]].test(arg[1]):
            self.files['pcl'].assign(self.files['pcl'].get()+1)
            self.cycle_counter+=1

    def rrf(self,arg):
        arg=list(map(lambda x: x.strip(),arg.split(',')))
        if(arg[1]=='f'):
            arg[1]=arg[0]
        else:
            if(arg[1]!='w'):
                print('Bad Styntax: Only F or W')
        self.files[arg[1]].assign(self.files[arg[0]].RRF())
        self.cycle_counter+=1

    def rlf(self,arg):
        arg=list(map(lambda x: x.strip(),arg.split(',')))
        if(arg[1]=='f'):
            arg[1]=arg[0]
        else:
            if(arg[1]!='w'):
                print('Bad Styntax: Only F or W')
        self.files[arg[1]].assign(self.files[arg[0]].RLF())
        self.cycle_counter+=1

    def incf(self,arg):
        arg=list(map(lambda x: x.strip(),arg.split(',')))
        if(arg[1]=='f'):
            arg[1]=arg[0]
        else:
            if(arg[1]!='w'):
                print('Bad Styntax: Only F or W')
        self.files[arg[1]].assign(self.files[arg[0]].inc())
        self.cycle_counter+=1

    def decf(self,arg):
        arg=list(map(lambda x: x.strip(),arg.split(',')))
        if(arg[1]=='f'):
            arg[1]=arg[0]
        else:
            if(arg[1]!='w'):
                print('Bad Styntax: Only F or W')
        self.files[arg[1]].assign(self.files[arg[0]].dec())
        self.cycle_counter+=1

    def decfsz(self,arg):
        self.cycle_counter+=1
        arg=list(map(lambda x: x.strip(),arg.split(',')))
        if(arg[1]=='f'):
            arg[1]=arg[0]
        else:
            if(arg[1]!='w'):
                print('Bad Styntax: Only F or W')

        temp_z=self.files['status'].test('z')
        self.files[arg[1]].assign(self.files[arg[0]].dec())

        if(self.files['status'].test('z')):
            self.cycle_counter+=1
            self.files['pcl'].assign(self.files['pcl'].get()+1)

        if(temp_z):
            self.files['status'].BSF('z')
        else:
            self.files['status'].BCF('z')


    def incfsz(self,arg):
        self.cycle_counter+=1
        arg=list(map(lambda x: x.strip(),arg.split(',')))
        if(arg[1]=='f'):
            arg[1]=arg[0]
        else:
            if(arg[1]!='w'):
                print('Bad Styntax: Only F or W')

        temp_z=self.files['status'].test('z')

        self.files[arg[1]].assign(self.files[arg[0]].inc())

        if(self.files['status'].test('z')):
            self.cycle_counter+=1
            self.files['pcl'].assign(self.files['pcl'].get()+1)

        if(temp_z):
            self.files['status'].BSF('z')
        else:
            self.files['status'].BCF('z')




    def movf(self,arg):
        self.cycle_counter+=1
        arg=list(map(lambda x: x.strip(),arg.split(',')))
        self.files[arg[1]].assign(self.files[arg[0]].get())
        if(self.files[arg[1]].get()==0):
            self.files['status'].BSF('z')
        else:
            self.files['status'].BCF('z')

    def movwf(self,arg):
        self.cycle_counter+=1
        self.files[arg].assign(self.files['w'].get())

    def movfw(self,arg):
        self.cycle_counter+=1
        self.files['w'].assign(self.files[arg].get())

    def movlw(self,arg):
        self.cycle_counter+=1
        self.files['w'].assign(self.parse_literal(arg))

    def nop(self):
        self.cycle_counter+=1

    def retlw(self,arg):
        self.cycle_counter+=2
        self.movlw(arg)
        self.retur()

    def END(self):
        return True

    def addwf(self,arg):
        self.cycle_counter+=1
        arg=list(map(lambda x: x.strip(),arg.split(',')))
        if(arg[1]=='f'):
            arg[1]=arg[0]
        else:
            if(arg[1]!='w'):
                print('Bad Styntax: Only F or W')
        self.files[arg[1]].assign(self.files[arg[0]].add(self.files['w']))

    def xorwf(self,arg):
        self.cycle_counter+=1
        arg=list(map(lambda x: x.strip(),arg.split(',')))
        if(arg[1]=='f'):
            arg[1]=arg[0]
        else:
            if(arg[1]!='w'):
                print('Bad Styntax: Only F or W')
        self.files[arg[1]].assign(self.files[arg[0]].xor(self.files['w']))

    def andwf(self,arg):
        self.cycle_counter+=1
        arg=list(map(lambda x: x.strip(),arg.split(',')))
        if(arg[1]=='f'):
            arg[1]=arg[0]
        else:
            if(arg[1]!='w'):
                print('Bad Styntax: Only F or W')
        self.files[arg[1]].assign(self.files[arg[0]].AND(self.files['w']))

    def iorwf(self,arg):
        self.cycle_counter+=1
        arg=list(map(lambda x: x.strip(),arg.split(',')))
        if(arg[1]=='f'):
            arg[1]=arg[0]
        else:
            if(arg[1]!='w'):
                print('Bad Styntax: Only F or W')
        self.files[arg[1]].assign(self.files[arg[0]].ior(self.files['w']))

    def subwf(self,arg):
        self.cycle_counter+=1
        arg=list(map(lambda x: x.strip(),arg.split(',')))
        if(arg[1]=='f'):
            arg[1]=arg[0]
        else:
            if(arg[1]!='w'):
                print('Bad Styntax: Only F or W')
        self.files[arg[1]].assign(self.files[arg[0]].sub(self.files['w']))

    def addlw(self,arg):
        self.cycle_counter+=1
        temp=filex(self)
        temp.assign(self.parse_literal(arg))
        self.files['w'].assign(temp.add(self.files['w']))

    def sublw(self,arg):
        self.cycle_counter+=1
        temp=filex(self)
        temp.assign(self.parse_literal(arg))
        self.files['w'].assign(temp.sub(self.files['w']))

    def xorlw(self,arg):
        self.cycle_counter+=1
        temp=filex(self)
        temp.assign(self.parse_literal(arg))
        self.files['w'].assign(temp.xor(self.files['w']))

    def iorlw(self,arg):
        self.cycle_counter+=1
        temp=filex(self)
        temp.assign(self.parse_literal(arg))
        self.files['w'].assign(temp.ior(self.files['w']))

    def andlw(self,arg):
        self.cycle_counter+=1
        temp=filex(self)
        temp.assign(self.parse_literal(arg))
        self.files['w'].assign(temp.AND(self.files['w']))


def run_PIC(registers,instruction):
    P=PIC(registers,instruction.split('\n'))
    while True:
        term=1
        while term:
            ok=True
            comand=input()
            if(comand.strip()!=''):
                ok=False
                try:
                    P.manual_exectue(comand)
                except Exception as e:
                    print('Encountered error in comand!')
                    print(traceback.format_exc())

            if ok:
                try:
                    term=2
                    try:
                        while term==2:
                            term=P.execute()
                    except KeyboardInterrupt:
                        P.bp_mode=0
                except:
                    print(P.files)
                    raise
        print('Program finished by reaching END opcode. Entering interactive mode')





