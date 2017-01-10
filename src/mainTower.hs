import Graphics.Gloss
main = display(InWindow"Tower"(1200,900)(10,10))white picture
picture=pictures$strt[1..6]
chng[][]=[]
chng(x:xs)[]=xs
chng[](y:_)=[y]
chng(x:xs)(y:_)=if x>y then y:x:xs else xs
f1 i n t1 t2 t3=(towers i t1 t2 t3)++(f2(i+1)(n+1)t1 t2 t3)
f2 i n t1 t2 t3
  |0==length t1&&0==length t2=[]
  |n`mod`3==1=f1 i n(chng t1 t3)t2(chng t3 t1)
  |n`mod`3==2=f1 i n t1(chng t2 t3)(chng t3 t2)
  |otherwise= f1 i n(chng t1 t2)(chng t2 t1)t3
towers i t1 t2 t3=[translate 0(i*(-110))$pictures[tower(-160)t1,tower 0 t2,tower 160 t3]]
tower x xs=pictures$[translate x((r*10)-r)$rectangleWire d(d*10)]++[translate x((indxOf) i*(-d)+l)$pictures[translate(-2)(-3)$scale 0.05 0.05$text$show$floor i,scale(3*i)1$circle r]|i<-xs]
  where r=5
        d=r*2
        l=d*(fromIntegral$length xs)
        indxOf i=fromIntegral.(+1).head$filter(\x->x<length xs&&xs!!x==i)[0..]
strt t=f1 1 2 t[][]
