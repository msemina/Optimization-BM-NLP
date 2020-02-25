C       M E T O Ñ  H ú û T O H A . B E P C à ü   2.                     
C                                                                       
      SUBROUTINE A8(N,X,A,B,F,AGR,AGS,Y,G1,Q,PAR,FNLP)                  
      COMMON/A81/ HES /A82/ P1                                          
     &      /A83/ P2  /A84/ XV                                          
     &      /A85/ FV  /A86/ XT                                          
     &      /A87/ TR  /A88/ L1                                          
     &      /A829/ M1  /C/    NF                                        
     &      /GRD/ Z1,Z2                                                 
      REAL*8   X(N),A(N),B(N),F,Y,G1(N),PAR(40),                        
     &         HES(1),P1(1),P2(1),XV(1),FV(1),XT(1),TR(1),L1(1),M1(1),  
     &         C,E,H,NG1,E1,E2,E3,Z1,Z2,Z3,Z4,ST,C0,C1,C2,C3,C4,CMIN,   
     &         FC,F1,F2,F3,F4,FMIN,F5,F6,Z                              
      INTEGER  D,K,KK,I,J,N1,N2,PODRP,SHAGP,INF,NF,Q                    
      LOGICAL  GRAT                                                     
      EXTERNAL F,FNLP                                                   
C-----------------------------------------------------------------------
      E=PAR(1+Q)                                                        
      D=PAR(2+Q)                                                        
      KK=PAR(3+Q)                                                       
      INF=PAR(4+Q)                                                      
      ST=PAR(5+Q)                                                       
      E1=PAR(6+Q)                                                       
      H=PAR(7+Q)                                                        
      SHAGP=PAR(8+Q)                                                    
      PODRP=PAR(9+Q)                                                    
      E2=1.D-11                                                         
      E3=1.D-18                                                         
      N1=1                                                              
      N2=N                                                              
      IF(E1.LT.0.000001D0.OR.E1.GT.0.499999D0) E1=0.499D0               
      NF=0                                                              
      DO 100 I=1,N                                                      
         XV(I)=X(I)                                                     
  100 CONTINUE                                                          
      IF(INF.EQ.0.OR.INF.EQ.2) Y=F(X,FNLP)                              
      CALL GRADT(N,F,NG1,X,G1,H,1,N,P2,FNLP)                            
      IF(PODRP.EQ.0) GO TO 1102                                         
C                                                                       
C          èEóATú àCXOÑHõX ÑAHHõX                                       
C                                                                       
      PRINT 1100                                                        
 1100 FORMAT(/5X,'MàHàMàáAñàü METOÑOM HúûTOHA.BEPCàü 2.')               
      PRINT 1101,N,E,D,ST,E1,H                                          
 1101 FORMAT(/5X,'PAáMEPHOCTú èPOCTPAHCTBA èEPEMEHHõX',2X,'N=',I3,      
     &       /5X,'TOóHOCTú PEòEHàü áAÑAóà',14X,'E=',D11.4,              
     &       /5X,'óàCãO àTEPAñàâ',23X,'D=',I5,                          
     &       /5X,'HAóAãúHõâ òAÉ CèìCKA',17X,'ST=',D11.4,                
     &       /5X,'MAÜOPAHTA ÉOãÑCTEâHA',17X,'E1=',D11.4,                
     &       /5X,'òAÉ ÑàîîEPEHñàPOBAHàü',17X,'H=',D11.4,/)              
 1102 CALL PRTUCM(0,NF,N,X,Y,NG1,SHAGP,PODRP)                           
      K=1                                                               
C                                                                       
C  H A ó A ã O   O C H O B H O É O   ñ à K ã A                          
C                                                                       
   32 IF(.NOT.(K.LE.D.AND.NG1.GT.E)) GO TO 33                           
      DO 140 J=N1,N2                                                    
          Z1=X(J)                                                       
          Z3=P2((J-1)*3+3)                                              
          X(J)=Z1+Z3                                                    
          HES((J-1)*N+J)=(P2((J-1)*3+1)-2*Y+P2((J-1)*3+2))/Z3/Z3        
          N3=J-1                                                        
          IF(N1.GT.N3) GO TO 77                                         
             DO 150 I=N1,N3                                             
                Z2=X(I)                                                 
                Z4=P2((I-1)*3+3)                                        
                X(I)=Z2+Z4                                              
                HES((J-1)*N+I)=(F(X,FNLP)-P2((J-1)*3+2)                 
     &                         -P2((I-1)*3+2)+Y)/Z3/Z4                  
                HES((I-1)*N+J)=HES((J-1)*N+I)                           
                X(I)=Z2                                                 
  150        CONTINUE                                                   
   77     CONTINUE                                                      
          X(J)=Z1                                                       
  140 CONTINUE                                                          
      CALL DMINV(HES,N,DET,L1,M1)                                       
      IF(DET.NE.0) GO TO 41                                             
          PRINT 42                                                      
   42     FORMAT(5X,'ÉECCàAH BõPOÜÑEH')                                 
          GRAT=.TRUE.                                                   
          GO TO 1                                                       
   41 GRAT=.FALSE.                                                      
      DO 170 I=N1,N2                                                    
          P1(I)=0                                                       
          DO 160 J=N1,N                                                 
             P1(I)=P1(I)+HES((J-1)*N+I)*G1(J)                           
  160     CONTINUE                                                      
  170 CONTINUE                                                          
      Z1=0                                                              
      DO 180 I=N1,N2                                                    
         Z1=Z1+G1(I)*P1(I)                                              
  180 CONTINUE                                                          
      C=-1.D0                                                           
      IF(Z1.GT.0) C=1.D0                                                
      DO 190 I=N1,N2                                                    
         XV(I)=X(I)-C*P1(I)                                             
  190 CONTINUE                                                          
      FC=F(XV,FNLP)                                                     
      Y1=Y-E1*C*Z1                                                      
      IF(FC.LT.Y1) GO TO 9                                              
      C0=1.D0                                                           
      DO 200 I=N1,N2                                                    
         Z1=DEXP(DLOG(E3+DABS(X(I)))+DLOG(E2)-DLOG(E3+DABS(P1(I))))     
         IF(Z1.LT.C0) C0=Z1                                             
  200 CONTINUE                                                          
      IF(C0.LT.E3) C0=E3                                                
      GO TO 2                                                           
    1        C0=ST                                                      
             C=ST                                                       
             DO 210 I=N1,N2                                             
              Z1=DEXP(DLOG(E3+DABS(X(I)))+DLOG(E2)-DLOG(E3+DABS(G1(I))))
              IF(Z1.LT.C0) C0=Z1                                        
              P1(I)=G1(I)                                               
              XV(I)=X(I)-C*P1(I)                                        
  210        CONTINUE                                                   
             IF(C0.LT.E3) C0=E3                                         
             FC=F(XV,FNLP)                                              
    2 C1=0                                                              
      F1=Y                                                              
      IF(.NOT.(FC.LT.Y)) GO TO 51                                       
         CMIN=C                                                         
         FMIN=FC                                                        
         C3=C*0.5                                                       
         DO 220 I=N1,N2                                                 
            XV(I)=X(I)-C3*P1(I)                                         
  220    CONTINUE                                                       
         F3=F(XV,FNLP)                                                  
         IF(F3.GT.FMIN) GO TO 6                                         
            C4=C                                                        
            F4=FC                                                       
            GO TO 4                                                     
   51 C3=C                                                              
      F3=FC                                                             
    3 C4=C3                                                             
      F4=F3                                                             
      C3=C3*0.1D0                                                       
      IF(.NOT.(DABS(C3).LT.DABS(C0))) GO TO 52                          
          IF(GRAT) GO TO 53                                             
            GRAT=.TRUE.                                                 
            GO TO 1                                                     
   53     PRINT 154                                                     
  154     FORMAT(10X,'OÑHOMEPHõâ èOàCK HEìÑAóEH')                       
          RETURN                                                        
   52 DO 230 I=N1,N2                                                    
          XV(I)=X(I)-C3*P1(I)                                           
  230 CONTINUE                                                          
      F3=F(XV,FNLP)                                                     
      IF(F3.GT.Y) GO TO 3                                               
    4       CMIN=C3                                                     
            FMIN=F3                                                     
    5       C2=C3*0.5D0                                                 
            IF(.NOT.(DABS(C2).LT.DABS(C0))) GO TO 54                    
                IF(GRAT) GO TO 55                                       
                   GRAT=.TRUE.                                          
                   GO TO 1                                              
   55           PRINT 155                                               
  155           FORMAT(10X,'OÑHOMEPHõâ èOàCK HEìÑAóEH')                 
                RETURN                                                  
   54      DO 240 I=N1,N2                                               
               XV(I)=X(I)-C2*P1(I)                                      
  240      CONTINUE                                                     
           F2=F(XV,FNLP)                                                
           IF(F2.GT.FMIN) GO TO 8                                       
               CMIN=C2                                                  
               FMIN=F2                                                  
               C4=C3                                                    
               F4=F3                                                    
               C3=C2                                                    
               F3=F2                                                    
               GO TO 5                                                  
    6 C2=C3                                                             
      F2=F3                                                             
      C3=C                                                              
      F3=FC                                                             
    7 C4=C3*2                                                           
      DO 250 I=N1,N2                                                    
         XV(I)=X(I)-C4*P1(I)                                            
  250 CONTINUE                                                          
      F4=F(XV,FNLP)                                                     
      IF(F4.GT.F3) GO TO 8                                              
         CMIN=C4                                                        
         FMIN=F4                                                        
         C1=C2                                                          
         F1=F2                                                          
         C2=C3                                                          
         F2=F3                                                          
         C3=C4                                                          
         F3=F4                                                          
         GO TO 7                                                        
    8 C1=C1/C3                                                          
      C4=C4/C3                                                          
      FC=(1-C1)*2*(C1-C4)/(.5-C4)*F2+2*(C1-.5)*(C1-C4)/(1-C4)*          
     &    F3+(C1-.5)/(C4-.5)*(C1-1)/(C4-1)*F4                           
      F5=(FC-F1)                                                        
      F6=(F1-F3)*0.0001D0                                               
      IF(.NOT.(DABS(F5).LT.DABS(F6))) GO TO 56                          
         Z1=F2*(1-C4)                                                   
         Z2=F3*(C4-.5)                                                  
         Z3=(-F4*.5)                                                    
         Z=Z1+Z2+Z3                                                     
         IF(DABS(Z).LT.E3) GO TO 60                                     
            C=(Z1*(1+C4)+Z2*(.5+C4)+Z3*1.5)/(Z1+Z2+Z3)*C3*.5            
            GO TO 205                                                   
   56 F1=(-.5*(C4-.5)*(C4-1)*F1)                                        
      F2=(1-C1)*(C4-C1)*(C4-1)*F2                                       
      F3=(-(.5-C1)*(C4-C1)*(C4-.5)*F3)                                  
      F4=.5*(.5-C1)*(1-C1)*F4                                           
      Z1=3*(F1+F2+F3+F4)                                                
      Z2=(-(F1*(1.5+C4)+F2*(C1+1+C4)+F3*(C1+.5+C4)+F4*(C1+1.5)))        
      Z3=F1*(.5+1.5*C4)+F2*(C1+C1*C4+C4)+F3*(C1*.5+C1*C4+.5*C4)+        
     &   F4*(C1*1.5+.5)                                                 
      IF(DABS(Z1).LT.1.D-15) GO TO 60                                   
            C=(DSQRT(DABS(Z2*Z2-Z1*Z3))-Z2)/Z1*C3                       
  205       CONTINUE                                                    
            DO 260 I=N1,N2                                              
               XV(I)=X(I)-C*P1(I)                                       
  260       CONTINUE                                                    
            FC=F(XV,FNLP)                                               
            IF(FMIN.GT.FC) GO TO 9                                      
   60 DO 270 I=N1,N2                                                    
         X(I)=X(I)-CMIN*P1(I)                                           
  270 CONTINUE                                                          
      Y=FMIN                                                            
      GO TO 10                                                          
    9     Y=FC                                                          
          DO 280 I=N1,N2                                                
             X(I)=XV(I)                                                 
  280     CONTINUE                                                      
   10 CALL GRADT(N,F,NG1,X,G1,H,1,N,P2,FNLP)                            
      CALL PRTUCM(K,NF,N,X,Y,NG1,SHAGP,PODRP)                           
      KK=K                                                              
      K=K+1                                                             
      GO TO 32                                                          
C                                                                       
C  K O H E ñ   O C H O B H O É O   ñ à K ã A                            
C                                                                       
   33 CONTINUE                                                          
      IF(PODRP.NE.0) PRINT 21                                           
   21 FORMAT(/,5X,'OèTàMAãúHAü TOóKA')                                  
      CALL PRTUCM(K,NF,N,X,Y,NG1,1,3)                                   
      PAR(3+Q)=KK                                                       
      RETURN                                                            
      END                                                               
C                                                                       
C                                                                       
C                                                                       
      SUBROUTINE GRADT(N,F,NG1,X,G1,H,N1,N2,P2,FNLP)                    
      COMMON/GRD/Z1,Z2                                                  
      REAL*8   X(N),P2(3,N),G1(N),                                      
     &         NG1,Z1,Z2,H,F                                            
      INTEGER  I,N1,N2                                                  
      EXTERNAL FNLP                                                     
      NG1=0                                                             
      DO 30 I=N1,N2                                                     
         Z1=X(I)                                                        
         Z2=H*(1.D0+0.001D0*DABS(Z1))                                   
         P2(3,I)=Z2                                                     
         X(I)=Z1+Z2                                                     
         P2(2,I)=F(X,FNLP)                                              
         X(I)=Z1-Z2                                                     
         P2(1,I)=F(X,FNLP)                                              
         G1(I)=(P2(2,I)-P2(1,I))/2/Z2                                   
         X(I)=Z1                                                        
         NG1=NG1+G1(I)**2                                               
   30 CONTINUE                                                          
      NG1=DSQRT(NG1)                                                    
      RETURN                                                            
      END                                                               
