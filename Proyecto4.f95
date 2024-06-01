program SimpsonIntegral  
    Implicit none
    
    !Declaramos las variables
    integer n,j, interval, k 
    real*8 x2,Xi,Xf,h,A1,sum, F,I, h_comp, xfn, xin

    !Leemos los límites
    write(*,*) 'Limite inferior, Limite superior:'
    Read(*,*) xi,xf

    n = 8 ! Numero de intervalos anidados simples, debe ser par
    interval = 100 ! Número de intervalos compuestos
     
    write(*,*) 'Numero de intervalos por metodo compuesto:'
    write(*,*) interval
     
    write(*,*) 'Numero de intervalos anidados del metodo simple:'
    write(*,*) n 

    ! Longitud del intervalo compuesto 
    h_comp = 1.d0*ABS(xf-xi)/interval

    ! Longitud del intervalo simple
    h = 1.0*h_comp/n !long. del intervalo 

    I=0 !integral empieza en cero

    xin = xi

    Do k=1,interval
        
        xfn = xin + h_comp

        A1=(h/3.0)*(f(xin)+f(xfn)) ! Evaluada en los extremos
        sum=0.0
        
        do j=1,n-1 !Depende del numero de intervalos
            x2=xin+j*h

            if (mod(j,2).eq.1) then  !Se cumple para numero impar
                sum=sum+4.0*f(x2)       ! Los terminos 1,3,5 tienen factor 4
            else
                sum=sum+2.0*f(x2)       !Se cumple para numero par
                             !terminos 2,4,6... tienen factor 2
            end if
    
        end do 

        I = I + (h/3.0*sum)+A1

        xin = xfn

        
    End do
    write(*,*) 'La integral de ', xi, 'a ', xf, 'es', I
end program

function f(x)
    Implicit none
    Real*8 x, f
!        f= (2.+cos(1.+x**1.5)*exp(0.5*x))/(sqrt(1.+0.5*sin(x)))
!       f=((x**3)+(4*(x**2))-10)
    f=atan(x)

    return
end function