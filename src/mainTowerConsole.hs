module Main where
chng[][]=[]
chng(x:xs)[]=xs
chng[](y:_)=[y]
chng(x:xs)(y:_)=if x>y then y:x:xs else xs
f1 i n t1 t2 t3=(f3 i t1 t2 t3)++(f2(i+1)(n+1)t1 t2 t3)
f2 i n t1 t2 t3
  |0==length t1&&0==length t2=[]
  |n`mod`3==1=f1 i n(chng t1 t3)t2(chng t3 t1)
  |n`mod`3==2=f1 i n t1(chng t2 t3)(chng t3 t2)
  |otherwise= f1 i n(chng t1 t2)(chng t2 t1)t3
f3 i t1 t2 t3=[-3]++[i]++[-1]++t1++[-2]++t2++[-2]++t3++[-2]
strt t=f1 1 2 t[][]
frmt=foldl1(++).map frmtEl where frmtEl x|x==(-1)=")"|x==(-2)="|"|x==(-3)="("|otherwise=show x
main = print.frmt$strt[1..6]
