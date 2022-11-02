program depu
implicit none 
integer::puke(52)=[114,102,103,104,105,106,107,108,109,110,111,112,113,&!1是红桃，2是方块，3是草花，4是黑桃
                           214,202,203,204,205,206,207,208,209,210,211,212,213,&!A输入14，没有1这张牌
                           314,302,303,304,305,306,307,308,309,310,311,312,313,&
                           414,402,403,404,405,406,407,408,409,410,411,412,413],yzp(7),yzp1(7)
integer::i,j,k,wanjia,sp1,sp2,hp1,hp2,hp3,hp4,hp5,pw,h,zzz,ying=0,ping=0,changci
integer,dimension(:,:),allocatable::wj
integer(kind=8),dimension(:),allocatable::win1,win2,f1,f2,ff2,winner
integer(kind=8)::fss0(2),fss(2),zuida1,zuida2
real::u
read(*,*)wanjia,sp1,sp2,hp1,hp2,hp3,hp4,hp5!没有的河牌就输入0
allocate(wj(wanjia,3))
allocate(win1(wanjia),win2(wanjia),f1(wanjia),f2(wanjia),ff2(wanjia),winner(wanjia))
yzp1=[sp1,sp2,hp1,hp2,hp3,hp4,hp5]
open(200,file='test.dat',status='unknown')
!场次循环
changci=100000
do i=1,changci
    pw=52;yzp=yzp1;win1=0;win2=0;winner=0;ff2=0;f1=0;f2=0
    call random_seed()
    call shuffle(puke)
!发手牌
    do k=1,wanjia-1 
        wj(k,1)=puke(k*2-1)
        wj(k,2)=puke(k*2)
        do while(any(yzp==wj(k,1)))
            wj(k,1)=puke(pw)
            pw=pw-1
        end do 
        do while(any(yzp==wj(k,2)))
            wj(k,2)=puke(pw)
            pw=pw-1
        end do 
    end do 
    wj(wanjia,1)=sp1;wj(wanjia,2)=sp2
!发河牌
    do k=3,7 
        if(yzp(k)==0)then
            zzz=puke(pw)
            pw=pw-1
            do while(any(yzp==zzz))
                zzz=puke(pw)
                pw=pw-1
            end do 
            yzp(k)=zzz 
        end if 
    end do 

    hp1=yzp(3);hp2=yzp(4);hp3=yzp(5);hp4=yzp(6);hp5=yzp(7)
!给每个人打分
    do k=1,wanjia 
    fss=0;fss0=0
        call pingfen(hp1,hp2,hp3,hp4,hp5,fss0(1),fss0(2))
            fss(1)=fss0(1);fss(2)=fss0(2)
        call pingfen(wj(k,1),hp2,hp3,hp4,hp5,fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(1)=fss0(1);fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        call pingfen(hp1,wj(k,1),hp3,hp4,hp5,fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(1)=fss0(1);fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        call pingfen(hp1,hp2,wj(k,1),hp4,hp5,fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(1)=fss0(1);fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        call pingfen(hp1,hp2,hp3,wj(k,1),hp5,fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(1)=fss0(1);fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        call pingfen(hp1,hp2,hp3,hp4,wj(k,1),fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        call pingfen(wj(k,2),hp2,hp3,hp4,hp5,fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        call pingfen(hp1,wj(k,2),hp3,hp4,hp5,fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(1)=fss0(1);fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        call pingfen(hp1,hp2,wj(k,2),hp4,hp5,fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(1)=fss0(1);fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        call pingfen(hp1,hp2,hp3,wj(k,2),hp5,fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(1)=fss0(1);fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        call pingfen(hp1,hp2,hp3,hp4,wj(k,2),fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(1)=fss0(1);fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        call pingfen(wj(k,1),wj(k,2),hp3,hp4,hp5,fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(1)=fss0(1);fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        call pingfen(wj(k,1),hp2,wj(k,2),hp4,hp5,fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(1)=fss0(1);fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if     
        call pingfen(wj(k,1),hp2,hp3,wj(k,2),hp5,fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(1)=fss0(1);fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        call pingfen(wj(k,1),hp2,hp3,hp4,wj(k,2),fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(1)=fss0(1);fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        call pingfen(hp1,wj(k,1),wj(k,2),hp4,hp5,fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(1)=fss0(1);fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(1)=fss0(1);fss(2)=fss0(2)
            end if
        end if 
        call pingfen(hp1,wj(k,1),hp3,wj(k,2),hp5,fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        call pingfen(hp1,wj(k,1),hp3,hp4,wj(k,2),fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(1)=fss0(1);fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        call pingfen(hp1,hp2,wj(k,1),wj(k,2),hp5,fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(1)=fss0(1);fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        call pingfen(hp1,hp2,wj(k,1),hp4,wj(k,2),fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(1)=fss0(1);fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        call pingfen(hp1,hp2,hp3,wj(k,1),wj(k,2),fss0(1),fss0(2))
        if(fss0(1)>fss(1))then 
            fss(1)=fss0(1);fss(2)=fss0(2)
        end if 
        if(fss0(1)==fss(1))then 
            if(fss0(2)>fss(2))then 
                fss(2)=fss0(2)
            end if
        end if 
        f1(k)=fss(1);f2(k)=fss(2)
    end do 
!开始判断输赢
    zuida1=maxval(f1)

    do k=1,wanjia 
        if(f1(k)==zuida1) win1(k)=win1(k)+1
    end do 
!只有一个人牌型最大
    if(sum(win1)==1)then 
        do k=1,wanjia 
            if(win1(k)==1) winner(k)=1
        end do 
    end if 
!有多人有大牌型
    if(sum(win1)/=1)then 
        do k=1,wanjia 
            if(win1(k)==1) ff2(k)=f2(k)
        end do 
        zuida2=maxval(ff2)
        do k=1,wanjia 
            if(f2(k)==zuida2) win2(k)=win2(k)+1
        end do 
    end if 
    do k=1,wanjia
        if(win2(k)==1) winner(k)=1 
    end do 
!只有一个人赢
    if(sum(winner)==1)then 
        if(winner(wanjia)==1) ying=ying+1
    end if 
    if(sum(winner)/=1)then 
        if(winner(wanjia)==1) ping=ping+1
    end if 

    

    write(200,*)pw 
    write(200,*)yzp 
    do k=1,wanjia 
        write(200,*)wj(k,1),wj(k,2),f1(k),f2(k),winner(k)
    end do 
    write(200,*) ying,ping
end do 
print*,ying*1.0/changci,ping*1.0/changci
close(200)

end program depu 

SUBROUTINE shuffle(D)
implicit none 
integer,dimension(52),intent(inout)::D
integer::i,p,t,j
real::r1,r2
do i=1,500
    call random_number(r1)
    call random_number(r2)
    p=floor(r1*52)+1
    j=floor(r2*52)+1
    t=D(p)
    D(p)=D(j)
    D(j)=t
end do 
end SUBROUTINE shuffle

!评分，对子加6分，两对12分，三条14分，顺子16分，12345的顺子15分，同花17分，四条25分，这样葫芦就是20分，同花顺33分
!分数分两个，第一个分数存牌型得分，四条的点数，三条的点数，两个对子的点数，一共十位，第二个分数为手牌从大到小排列
SUBROUTINE pingfen(pk1,pk2,pk3,pk4,pk5,fs1,fs2)
integer,intent(in)::pk1,pk2,pk3,pk4,pk5
integer(kind=8),intent(out)::fs1,fs2
integer::a(5),t1,t2,pk(13),hs(4),i,m
a(1)=pk1;a(2)=pk2;a(3)=pk3;a(4)=pk4;a(5)=pk5
fs1=0;fs2=0;pk=0;hs=0;dz=0
do i=1,5 
    do m=1,13
        if(mod(a(i),100)==(m+1)) pk(m)=pk(m)+1
    end do 
end do 
do i=1,5
    do m=1,4
        if(floor(a(i)/100.0)==m) hs(m)=hs(m)+1
    end do 
end do 
!找四条三条对子
do m=1,13 
    if(pk(m)==2.and.dz==0)then 
        fs1=fs1+6d10+m*1d2
        dz=1
        cycle
    end if 
    if(pk(m)==2.and.dz==1)then 
        fs1=fs1+6d10+m*1d4
    end if 
    if(pk(m)==3) fs1=fs1+14d10+m*1d6
    if(pk(m)==4) fs1=fs1+25d10+m*1d8
end do 
!找同花
do m=1,4
    if(hs(m)==5) fs1=fs1+17d10
end do 
!提取点数，排序
do i=1,5
    a(i)=mod(a(i),100)
end do 
do i=1,4
    do m=i+1,5 
        if(a(i)>a(m))then 
            t1=a(i)
            t2=a(m)
            a(i)=t2 
            a(m)=t1
        end if 
    end do 
end do 
fs2=a(5)*1d10+a(4)*1d8+a(3)*1d6+a(2)*1d4+a(1)*1d2
!找顺子
if((a(1)+1)==a(2).and.(a(2)+1)==a(3).and.(a(3)+1)==a(4).and.(a(4)+1)==a(5)) fs1=fs1+16d10  
if(a(1)==2.and.a(2)==3.and.a(3)==4.and.a(5)==14) fs1=fs1+15d10 
end SUBROUTINE pingfen