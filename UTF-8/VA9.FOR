C                                                                       
C              TECTИPOBAHИE METOДOB ИЗ ПAKETA БM                        
C              METOД   ЭЛЛИПCOИДOB     ( A9  )                          
C                                                                       
C        OПИCAHИE ПAPAMETPOB ПPOГPAMMЫ METOДA                           
C                                                                       
C      PAЗMEPHOCTЬ MACCИBOB X,A,B,G1  PABHA  N                          
         REAL *8 X(2),A(2),B(2),G1(2)                                   
         REAL *8 PAR(40),Y                                              
         REAL *8 F,FNLP                                                 
         EXTERNAL F,FNLP,GRAD,AGS                                       
         INTEGER Q                                                      
C                                                                       
C           OПИCAHИE OБЩИX OБЛACTEЙ METOДA                              
C                                                                       
         COMMON /A91/ BG/A92 / G /A93 / G2/A94 / GN                     
     *          /A95 / XN /A96/ X1                                      
C       PAЗMEPHOCTЬ  MACCИBA BG   = N*N                                 
         REAL *8 BG  (2 ,2)                                             
C       PAЗMEPHOCTЬ  BCEX MACCИBOB  = N                                 
         REAL *8 G ( 2 ),G2( 2 ),GN( 2 ),XN( 2 ),X1(2)                  
C                                                                       
C           XAPAKTEPИCTИKA ПAPAMETPOB ПPOГPAMMЫ METOДA                  
C                                                                       
C     N - PAЗMEPHOCTЬ ЗAДAЧИ                                            
C     X - BEKTOP УПPABЛЯEMЫX ПEPEMEHHЫX                                 
C     A - BEKTOP ЛEBЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ. ПEPEMEHHЫX              
C     B - BEKTOP ПPABЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ.ПEPEMEHHЫX              
C     F - ИMЯ ПOДПPOГPAMMЫ TИПA FUNCTION ДЛЯ BЫЧИCЛEHИЯ                 
C         ЗHAЧEHИЯ KPИTEPИЯ                                             
C     GRAD - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ                       
C         BЫЧИCЛEHИЯ ГPAДИEHTA                                          
C     AGS - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ PACЧETA                
C           MATPИЦЫ BTOPЫX ПPOИЗBOДHЫX (HE ИCПOЛЬЗУETCЯ )               
C     Y - ЗHAЧEHИE KPИTEPИЯ                                             
C     G1 - BEKTOP ЗHAЧEHИЙ ПPOИЗBOДHЫX ФУHKЦИИ F                        
C     Q - ПAPAMETP C ФИKCИPOBAHHЫM ЗHAЧEHИEM ( = 0)                     
C     PAR - BEKTOP ПAPAMETPOB METOДA                                    
C     FNLP - ФИKCИPOBAHHOE ИMЯ ПOДПPOГPAMMЫ                             
C                                                                       
C         ИCXOДHЫE ДAHHЫE ЗAДAЧИ                                        
C                                                                       
C     PAЗMEPHOCTЬ ЗAДAЧИ                                                
         N=2                                                            
C     HAЧAЛЬHAЯ TOЧKA                                                   
         X(1)=  1.2                                                     
         X(2)=  1.0                                                     
C                                                                       
C    ЗAДAHИE ПAPAMETPOB METOДA                                          
C                                                                       
         Q=0                                                            
C     TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ ПO HOPME ГPAДИEHTA                         
         PAR(Q+1)=0.00001                                               
C     MAKCИMAЛЬHOE ЧИCЛO ШAГOB,KOTOPOE MOЖHO CДEЛATЬ                     
         PAR(Q+2)=25                                                    
C     BЫXOДHOЙ ПAPAMETP                                                 
         PAR(Q+3)=0                                                     
C     PAДИУC HAЧAЛЬHOГO ШAPA,OПPEДEЛЯЮЩEГO OБЛACTЬ ПOИCKA                
         PAR(Q+4)=10.                                                   
C    TOЧHOCTЬ PEШEHИЯ OДHOMEPHOЙ ЗAДAЧИ BЫБOPA ШAГA                     
         PAR(Q+5)=0.001                                                 
C    MИHИMAЛЬHO ДOПУCTИMЫЙ OБ'EM ЭЛЛИПCOИДA                             
         PAR(Q+6)=0.0000000000000001                                    
C    HOMEP BEPCИИ METOДA ( =1 ДЛЯ BЫПУKЛЫX ФУHKЦИЙ ; =2, ECЛИ           
C                           ФУHKЦИЯ HEBЫПУKЛAЯ )                        
         PAR(Q+7)=2                                                     
C     ПOPЯДOK ДИФФEPEHЦИPOBAHИЯ ( = 1 ИЛИ 2 )                           
         PAR(Q+8)=1                                                     
C     ШAГ ДИФФEPEHЦИPOBAHИЯ                                              
         PAR(Q+9)=0.00001                                               
C     ЧИCЛO УДAЧHЫX ШAГOB,ЧEPEЗ KOTOPOE  CЛEДУET BЫBOДИTЬ                
C     ИHФOPMAЦИЮ                                                        
         PAR(Q+10)=1                                                    
C     CTEПEHЬ ПOДPOБHOCTИ BЫBOДИMOЙ ИHФOPMAЦИИ ( OT 0 ДO 3 )            
         PAR(Q+11)=3                                                    
C                                                                       
         CALL A9 (N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)                   
         END                                                            
