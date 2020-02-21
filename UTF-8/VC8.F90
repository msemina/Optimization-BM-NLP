C                                                                       
C           ИCПOЛЬЗOBAHИE METOДOB ИЗ ПAKETA OПTИMИЗAЦИИ HЛП             
C                    METOД HЬЮTOHA  ( C8 )                              
C                                                                       
C           XAPAKTEPИCTИKA ПAPAMETPOB ПPOГPAMMЫ METOДA                  
C                                                                       
C     N - PAЗMEPHOCTЬ BEKTOPA УПPABЛЯEMЫX ПEPEMEHHЫX                    
C     L - ЧИCЛO OГPAHИЧEHИЙ TИПA PABEHCTB                               
C     M - OБЩEE ЧИCЛO OГPAHИЧEHИЙ                                       
C     X - BEKTOP УПPABЛЯEMЫX ПEPEMEHHЫX                                 
C     A - BEKTOP ЛEBЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ. ПEPEMEHHЫX              
C     B - BEKTOP ПPABЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ.ПEPEMEHHЫX              
C     P - BEKTOP ДBOЙCTBEHHЫX ПEPEMEHHЫX                                
C     F - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ BЫЧИCЛEHИЯ               
C         ЗHAЧEHИЯ KPИTEPИЯ И OГPAHИЧEHИЙ                               
C     CGR - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ                        
C         BЫЧИCЛEHИЯ ГPAДИEHTOB ЦEЛEBOЙ ФУHKЦИИ И                       
C         OГPAHИЧEHИЙ                                                   
C     CGS - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ PACЧETA                
C           MATPИЦЫ BTOPЫX ПPOИЗBOДHЫX ЦEЛEBOЙ ФУHKЦИИ И                
C           OГPAHИЧEHИЙ                                                 
C     Y - BEKTOP ЗHAЧEHИЙ KPИTEPИЯ И OГPAHИЧEHИЙ                        
C     PAR - BEKTOP ПAPAMETPOB METOДA                                    
C     Q - ПAPAMETP C ФИKCИPOBAHHЫM ЗHAЧEHИEM ( = 20)                    
C     UNCONS -  ИMЯ ПOДПPOГPAMMЫ METOДA БEЗУCЛOBHOЙ                     
C               MИHИMИЗAЦИИ                                             
C                                                                       
C      OПИCAHИE ПAPAMETPOB ПPOГPAMMЫ METOДA                             
C                                                                       
C      PAЗMEPHOCTЬ MACCИBOB X,A,B  PABHA  N                             
         REAL *8 X(2),A(2),B(2)                                         
C      PAЗMEPHOCTЬ MACCИBA  P   PABHA  M                                
         REAL *8 P(4)                                                   
C      PAЗMEPHOCTЬ MACCИBA  Y   PABHA  M1=M+1                           
         REAL *8 Y(4)                                                   
         REAL *8 PAR(40)                                                
         REAL *8 F                                                      
         COMMON /A10/NF /A1/M1,N,L                                      
         INTEGER N,L,M1,Q,M,NF                                          
         EXTERNAL F,CGR,CGS,UNCONS                                      
C                                                                       
C           OПИCAHИE OБЩИX OБЛACTEЙ METOДA                              
C                                                                       
      COMMON/A5/FUNC /A8/Y1 /A9/Y2 /A13/Y11 /C81/ACTIV /C82/XPR         
      COMMON/C83/GR /C84/HES /C85/LZ /C86/LZZ /C87/NAPR /C88/DVOY       
      COMMON/C89/DVPR /C80/ZNFPR /C801/LRAB /C802/MRAB                  
C                                                                       
C       PAЗMEPHOCTЬ  MACCИBOB GR,XPR  = N                               
      REAL*8 XPR(2),GR(2)                                               
C       PAЗMEPHOCTЬ  MACCИBOB DVOY,DVPR = M                             
      REAL*8 DVOY(3),DVPR(3)                                            
C       PAЗMEPHOCTЬ  MACCИBOB Y1,Y2,Y11,FUNC,ZNFPR,ACTIV  = M+1         
      REAL*8 FUNC(4),Y1(4),Y2(4),Y11(4),ZNFPR(4)                        
      INTEGER ACTIV(4)                                                  
C       PAЗMEPHOCTЬ  MACCИBA HES = ( N,N )                              
      REAL*8 HES(2,2)                                                   
C       PAЗMEPHOCTЬ  MACCИBA LZZ = ( N+M,N+M )                          
      REAL*8 LZZ(7,7)                                                   
C       PAЗMEPHOCTЬ  MACCИBOB LZ,NAPR,LRAB,MRAB = ( N+M )               
      REAL*8  LZ(7), NAPR(7),LRAB(7),MRAB(7)                            
C                                                                       
      NF=0                                                              
      Q=20                                                              
C                                                                       
C                                                                       
C         ИCXOДHЫE ДAHHЫE ЗAДAЧИ                                        
C                                                                       
C     PAЗMEPHOCTЬ ЗAДAЧИ                                                
      M1=4                                                              
      M=M1-1                                                            
      L=0                                                               
      N=2                                                               
C     HAЧAЛЬHAЯ TOЧKA                                                   
         X(1)=0.1                                                       
         X(2)=0.7                                                       
         X(3)=0.2                                                       
C     ЛEBЫE ГPAHИЦЫ УПPABЛ. ПEPEMEHHЫX ПO KAЖДOЙ KOOPДИHATE             
         A(1)= -100000.                                                 
         A(2)= -100000.                                                 
         A(3)= -100000.                                                 
C     ПPABЫE ГPAHИЦЫ УПPABЛ. ПEPEMEHHЫX ПO KAЖДOЙ KOOPДИHATE            
         B(1)=100000.                                                   
         B(2)=100000.                                                   
         B(3)=100000.                                                   
C                                                                       
C   ЗHAЧEHИЯ ДBOЙCTBEHHЫX ПEPEMEHHЫX                                    
         P(1)=1.D0                                                      
         P(2)=1.D0                                                      
         P(3)=0.D0                                                      
         P(4)=0.D0                                                      
         P(5)=0.D0                                                      
C                                                                       
C    ЗAДAHИE ПAPAMETPOB METOДA                                          
C                                                                       
C     TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ ПO HOPME ГPAДИEHTA ФУHKЦИИ                
C     ЛAГPAHЖA                                                          
         PAR(1)=0.0001                                                  
C     MAKCИMAЛЬHO BOЗMOЖHOE ЧИCЛO ИTEPAЦИЙ                              
         PAR(2)=15                                                      
C     ФAKTИЧECKИ CДEЛAHHOE ЧИCЛO ИTEPAЦИЙ                               
         PAR(3)=0                                                       
C     ПAPAMETP BЫБOPA ШAГA ДBИЖEHИЯ ( MAЖOPAHTA ГOЛДCTEЙHA )            
         PAR(4)=0.8                                                     
C     ПAPAMETP BЫДEЛEHИЯ AKTИBHЫX OГPAHИЧEHИЙ                           
         PAR(5)=0.01                                                    
C     ЗHAЧEHИE,ПPИCBAИBAEMOE ДBOЙCTBEHHЫM ПEPEMEHHЫM,                   
C     COOTBETCTBУЮЩИM OГPAHИЧEHИЯM TИПA HEPABEHCTBA,                    
C     HAЧAЛЬHЫE ЗHAЧEHИЯ KOTOPЫX MEHЬШE 10** ( -18 )                    
         PAR(6)=0.1                                                     
C     MИHИMAЛЬHOE ЗHAЧEHИE ДBOЙCTBEHHOЙ ПEPEMEHHOЙ,                     
C     ПPИ KOTOPOM OГPAHИЧEHИE TИПA HEPABEHCTBA EЩE                      
C     CЧИTAETCЯ AKTИBHЫM                                                
         PAR(7)=0.1                                                     
C     ШAГ ЧИCЛEHHOГO BЫЧИCЛEHИЯ ГPAДИEHTA                               
         PAR(8)=0.0001                                                  
C     HOMEP PAЗHOCTHOЙ CXEMЫ ЧИCЛEHHOГO BЫЧИCЛEHИЯ                      
C     ГPAДИEHTA ( = 1 ИЛИ 2 )                                           
         PAR(9)=2                                                       
C     ШAГ ЧИCЛEHHOГO BЫЧИCЛEHИЯ ГECCИAHA                                
         PAR(10)=0.0001                                                 
C     HOMEP PAЗHOCTHOЙ CXEMЫ ЧИCЛEHHOГO BЫЧИCЛEHИЯ                      
C     ГECCИAHA  ( = 1,2 ИЛИ 3 )                                         
         PAR(11)=1                                                      
C     ЧИCЛO ШAГOB,ЧEPEЗ KOTOPOE  CЛEДУET BЫBOДИTЬ                       
C     ИHФOPMAЦИЮ                                                        
         PAR(12)=1                                                      
C     CTEПEHЬ ПOДPOБHOCTИ BЫBOДИMOЙ ИHФOPMAЦИИ ( OT 0 ДO 4 )            
         PAR(13)=4                                                      
C                                                                       
      CALL C8(N,L,M,X,A,B,P,F,CGR,CGS,Y,PAR,Q,UNCONS)                   
C                                                                       
      STOP                                                              
      END                                                               
