C                                                                       
C              TECTÈPOBAHÈE METOÄOB ÈÇ ÏAKETA ÁM                        
C              METOÄ   ÝËËÈÏCOÈÄOB     ( A9  )                          
C                                                                       
C        OÏÈCAHÈE ÏAPAMETPOB ÏPOÃPAMMÛ METOÄA                           
C                                                                       
C      PAÇMEPHOCTÜ MACCÈBOB X,A,B,G1  PABHA  N                          
         REAL *8 X(2),A(2),B(2),G1(2)                                   
         REAL *8 PAR(40),Y                                              
         REAL *8 F,FNLP                                                 
         EXTERNAL F,FNLP,GRAD,AGS                                       
         INTEGER Q                                                      
C                                                                       
C           OÏÈCAHÈE OÁÙÈX OÁËACTEÉ METOÄA                              
C                                                                       
         COMMON /A91/ BG/A92 / G /A93 / G2/A94 / GN                     
     *          /A95 / XN /A96/ X1                                      
C       PAÇMEPHOCTÜ  MACCÈBA BG   = N*N                                 
         REAL *8 BG  (2 ,2)                                             
C       PAÇMEPHOCTÜ  BCEX MACCÈBOB  = N                                 
         REAL *8 G ( 2 ),G2( 2 ),GN( 2 ),XN( 2 ),X1(2)                  
C                                                                       
C           XAPAKTEPÈCTÈKA ÏAPAMETPOB ÏPOÃPAMMÛ METOÄA                  
C                                                                       
C     N - PAÇMEPHOCTÜ ÇAÄA×È                                            
C     X - BEKTOP ÓÏPABËßEMÛX ÏEPEMEHHÛX                                 
C     A - BEKTOP ËEBÛX ÃPAHÈÖ ÈÇMEHEHÈß ÓÏPABË. ÏEPEMEHHÛX              
C     B - BEKTOP ÏPABÛX ÃPAHÈÖ ÈÇMEHEHÈß ÓÏPABË.ÏEPEMEHHÛX              
C     F - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA FUNCTION ÄËß BÛ×ÈCËEHÈß                 
C         ÇHA×EHÈß KPÈTEPÈß                                             
C     GRAD - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA SUBROUTINE ÄËß                       
C         BÛ×ÈCËEHÈß ÃPAÄÈEHTA                                          
C     AGS - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA SUBROUTINE ÄËß PAC×ETA                
C           MATPÈÖÛ BTOPÛX ÏPOÈÇBOÄHÛX (HE ÈCÏOËÜÇÓETCß )               
C     Y - ÇHA×EHÈE KPÈTEPÈß                                             
C     G1 - BEKTOP ÇHA×EHÈÉ ÏPOÈÇBOÄHÛX ÔÓHKÖÈÈ F                        
C     Q - ÏAPAMETP C ÔÈKCÈPOBAHHÛM ÇHA×EHÈEM ( = 0)                     
C     PAR - BEKTOP ÏAPAMETPOB METOÄA                                    
C     FNLP - ÔÈKCÈPOBAHHOE ÈMß ÏOÄÏPOÃPAMMÛ                             
C                                                                       
C         ÈCXOÄHÛE ÄAHHÛE ÇAÄA×È                                        
C                                                                       
C     PAÇMEPHOCTÜ ÇAÄA×È                                                
         N=2                                                            
C     HA×AËÜHAß TO×KA                                                   
         X(1)=  1.2                                                     
         X(2)=  1.0                                                     
C                                                                       
C    ÇAÄAHÈE ÏAPAMETPOB METOÄA                                          
C                                                                       
         Q=0                                                            
C     TO×HOCTÜ PEØEHÈß ÇAÄA×È ÏO HOPME ÃPAÄÈEHTA                         
         PAR(Q+1)=0.00001                                               
C     MAKCÈMAËÜHOE ×ÈCËO ØAÃOB,KOTOPOE MOÆHO CÄEËATÜ                     
         PAR(Q+2)=25                                                    
C     BÛXOÄHOÉ ÏAPAMETP                                                 
         PAR(Q+3)=0                                                     
C     PAÄÈÓC HA×AËÜHOÃO ØAPA,OÏPEÄEËßÞÙEÃO OÁËACTÜ ÏOÈCKA                
         PAR(Q+4)=10.                                                   
C    TO×HOCTÜ PEØEHÈß OÄHOMEPHOÉ ÇAÄA×È BÛÁOPA ØAÃA                     
         PAR(Q+5)=0.001                                                 
C    MÈHÈMAËÜHO ÄOÏÓCTÈMÛÉ OÁ'EM ÝËËÈÏCOÈÄA                             
         PAR(Q+6)=0.0000000000000001                                    
C    HOMEP BEPCÈÈ METOÄA ( =1 ÄËß BÛÏÓKËÛX ÔÓHKÖÈÉ ; =2, ECËÈ           
C                           ÔÓHKÖÈß HEBÛÏÓKËAß )                        
         PAR(Q+7)=2                                                     
C     ÏOPßÄOK ÄÈÔÔEPEHÖÈPOBAHÈß ( = 1 ÈËÈ 2 )                           
         PAR(Q+8)=1                                                     
C     ØAÃ ÄÈÔÔEPEHÖÈPOBAHÈß                                              
         PAR(Q+9)=0.00001                                               
C     ×ÈCËO ÓÄA×HÛX ØAÃOB,×EPEÇ KOTOPOE  CËEÄÓET BÛBOÄÈTÜ                
C     ÈHÔOPMAÖÈÞ                                                        
         PAR(Q+10)=1                                                    
C     CTEÏEHÜ ÏOÄPOÁHOCTÈ BÛBOÄÈMOÉ ÈHÔOPMAÖÈÈ ( OT 0 ÄO 3 )            
         PAR(Q+11)=3                                                    
C                                                                       
         CALL A9 (N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)                   
         END                                                            
