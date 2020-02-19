C                                                                       
C           ÈCÏOËÜÇOBAHÈE METOÄOB ÈÇ ÏAKETA OÏTÈMÈÇAÖÈÈ HËÏ             
C                    METOÄ HÜÞTOHA  ( C8 )                              
C                                                                       
C           XAPAKTEPÈCTÈKA ÏAPAMETPOB ÏPOÃPAMMÛ METOÄA                  
C                                                                       
C     N - PAÇMEPHOCTÜ BEKTOPA ÓÏPABËßEMÛX ÏEPEMEHHÛX                    
C     L - ×ÈCËO OÃPAHÈ×EHÈÉ TÈÏA PABEHCTB                               
C     M - OÁÙEE ×ÈCËO OÃPAHÈ×EHÈÉ                                       
C     X - BEKTOP ÓÏPABËßEMÛX ÏEPEMEHHÛX                                 
C     A - BEKTOP ËEBÛX ÃPAHÈÖ ÈÇMEHEHÈß ÓÏPABË. ÏEPEMEHHÛX              
C     B - BEKTOP ÏPABÛX ÃPAHÈÖ ÈÇMEHEHÈß ÓÏPABË.ÏEPEMEHHÛX              
C     P - BEKTOP ÄBOÉCTBEHHÛX ÏEPEMEHHÛX                                
C     F - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA SUBROUTINE ÄËß BÛ×ÈCËEHÈß               
C         ÇHA×EHÈß KPÈTEPÈß È OÃPAHÈ×EHÈÉ                               
C     CGR - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA SUBROUTINE ÄËß                        
C         BÛ×ÈCËEHÈß ÃPAÄÈEHTOB ÖEËEBOÉ ÔÓHKÖÈÈ È                       
C         OÃPAHÈ×EHÈÉ                                                   
C     CGS - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA SUBROUTINE ÄËß PAC×ETA                
C           MATPÈÖÛ BTOPÛX ÏPOÈÇBOÄHÛX ÖEËEBOÉ ÔÓHKÖÈÈ È                
C           OÃPAHÈ×EHÈÉ                                                 
C     Y - BEKTOP ÇHA×EHÈÉ KPÈTEPÈß È OÃPAHÈ×EHÈÉ                        
C     PAR - BEKTOP ÏAPAMETPOB METOÄA                                    
C     Q - ÏAPAMETP C ÔÈKCÈPOBAHHÛM ÇHA×EHÈEM ( = 20)                    
C     UNCONS -  ÈMß ÏOÄÏPOÃPAMMÛ METOÄA ÁEÇÓCËOBHOÉ                     
C               MÈHÈMÈÇAÖÈÈ                                             
C                                                                       
C      OÏÈCAHÈE ÏAPAMETPOB ÏPOÃPAMMÛ METOÄA                             
C                                                                       
C      PAÇMEPHOCTÜ MACCÈBOB X,A,B  PABHA  N                             
         REAL *8 X(2),A(2),B(2)                                         
C      PAÇMEPHOCTÜ MACCÈBA  P   PABHA  M                                
         REAL *8 P(4)                                                   
C      PAÇMEPHOCTÜ MACCÈBA  Y   PABHA  M1=M+1                           
         REAL *8 Y(4)                                                   
         REAL *8 PAR(40)                                                
         REAL *8 F                                                      
         COMMON /A10/NF /A1/M1,N,L                                      
         INTEGER N,L,M1,Q,M,NF                                          
         EXTERNAL F,CGR,CGS,UNCONS                                      
C                                                                       
C           OÏÈCAHÈE OÁÙÈX OÁËACTEÉ METOÄA                              
C                                                                       
      COMMON/A5/FUNC /A8/Y1 /A9/Y2 /A13/Y11 /C81/ACTIV /C82/XPR         
      COMMON/C83/GR /C84/HES /C85/LZ /C86/LZZ /C87/NAPR /C88/DVOY       
      COMMON/C89/DVPR /C80/ZNFPR /C801/LRAB /C802/MRAB                  
C                                                                       
C       PAÇMEPHOCTÜ  MACCÈBOB GR,XPR  = N                               
      REAL*8 XPR(2),GR(2)                                               
C       PAÇMEPHOCTÜ  MACCÈBOB DVOY,DVPR = M                             
      REAL*8 DVOY(3),DVPR(3)                                            
C       PAÇMEPHOCTÜ  MACCÈBOB Y1,Y2,Y11,FUNC,ZNFPR,ACTIV  = M+1         
      REAL*8 FUNC(4),Y1(4),Y2(4),Y11(4),ZNFPR(4)                        
      INTEGER ACTIV(4)                                                  
C       PAÇMEPHOCTÜ  MACCÈBA HES = ( N,N )                              
      REAL*8 HES(2,2)                                                   
C       PAÇMEPHOCTÜ  MACCÈBA LZZ = ( N+M,N+M )                          
      REAL*8 LZZ(7,7)                                                   
C       PAÇMEPHOCTÜ  MACCÈBOB LZ,NAPR,LRAB,MRAB = ( N+M )               
      REAL*8  LZ(7), NAPR(7),LRAB(7),MRAB(7)                            
C                                                                       
      NF=0                                                              
      Q=20                                                              
C                                                                       
C                                                                       
C         ÈCXOÄHÛE ÄAHHÛE ÇAÄA×È                                        
C                                                                       
C     PAÇMEPHOCTÜ ÇAÄA×È                                                
      M1=4                                                              
      M=M1-1                                                            
      L=0                                                               
      N=2                                                               
C     HA×AËÜHAß TO×KA                                                   
         X(1)=0.1                                                       
         X(2)=0.7                                                       
         X(3)=0.2                                                       
C     ËEBÛE ÃPAHÈÖÛ ÓÏPABË. ÏEPEMEHHÛX ÏO KAÆÄOÉ KOOPÄÈHATE             
         A(1)= -100000.                                                 
         A(2)= -100000.                                                 
         A(3)= -100000.                                                 
C     ÏPABÛE ÃPAHÈÖÛ ÓÏPABË. ÏEPEMEHHÛX ÏO KAÆÄOÉ KOOPÄÈHATE            
         B(1)=100000.                                                   
         B(2)=100000.                                                   
         B(3)=100000.                                                   
C                                                                       
C   ÇHA×EHÈß ÄBOÉCTBEHHÛX ÏEPEMEHHÛX                                    
         P(1)=1.D0                                                      
         P(2)=1.D0                                                      
         P(3)=0.D0                                                      
         P(4)=0.D0                                                      
         P(5)=0.D0                                                      
C                                                                       
C    ÇAÄAHÈE ÏAPAMETPOB METOÄA                                          
C                                                                       
C     TO×HOCTÜ PEØEHÈß ÇAÄA×È ÏO HOPME ÃPAÄÈEHTA ÔÓHKÖÈÈ                
C     ËAÃPAHÆA                                                          
         PAR(1)=0.0001                                                  
C     MAKCÈMAËÜHO BOÇMOÆHOE ×ÈCËO ÈTEPAÖÈÉ                              
         PAR(2)=15                                                      
C     ÔAKTÈ×ECKÈ CÄEËAHHOE ×ÈCËO ÈTEPAÖÈÉ                               
         PAR(3)=0                                                       
C     ÏAPAMETP BÛÁOPA ØAÃA ÄBÈÆEHÈß ( MAÆOPAHTA ÃOËÄCTEÉHA )            
         PAR(4)=0.8                                                     
C     ÏAPAMETP BÛÄEËEHÈß AKTÈBHÛX OÃPAHÈ×EHÈÉ                           
         PAR(5)=0.01                                                    
C     ÇHA×EHÈE,ÏPÈCBAÈBAEMOE ÄBOÉCTBEHHÛM ÏEPEMEHHÛM,                   
C     COOTBETCTBÓÞÙÈM OÃPAHÈ×EHÈßM TÈÏA HEPABEHCTBA,                    
C     HA×AËÜHÛE ÇHA×EHÈß KOTOPÛX MEHÜØE 10** ( -18 )                    
         PAR(6)=0.1                                                     
C     MÈHÈMAËÜHOE ÇHA×EHÈE ÄBOÉCTBEHHOÉ ÏEPEMEHHOÉ,                     
C     ÏPÈ KOTOPOM OÃPAHÈ×EHÈE TÈÏA HEPABEHCTBA EÙE                      
C     C×ÈTAETCß AKTÈBHÛM                                                
         PAR(7)=0.1                                                     
C     ØAÃ ×ÈCËEHHOÃO BÛ×ÈCËEHÈß ÃPAÄÈEHTA                               
         PAR(8)=0.0001                                                  
C     HOMEP PAÇHOCTHOÉ CXEMÛ ×ÈCËEHHOÃO BÛ×ÈCËEHÈß                      
C     ÃPAÄÈEHTA ( = 1 ÈËÈ 2 )                                           
         PAR(9)=2                                                       
C     ØAÃ ×ÈCËEHHOÃO BÛ×ÈCËEHÈß ÃECCÈAHA                                
         PAR(10)=0.0001                                                 
C     HOMEP PAÇHOCTHOÉ CXEMÛ ×ÈCËEHHOÃO BÛ×ÈCËEHÈß                      
C     ÃECCÈAHA  ( = 1,2 ÈËÈ 3 )                                         
         PAR(11)=1                                                      
C     ×ÈCËO ØAÃOB,×EPEÇ KOTOPOE  CËEÄÓET BÛBOÄÈTÜ                       
C     ÈHÔOPMAÖÈÞ                                                        
         PAR(12)=1                                                      
C     CTEÏEHÜ ÏOÄPOÁHOCTÈ BÛBOÄÈMOÉ ÈHÔOPMAÖÈÈ ( OT 0 ÄO 4 )            
         PAR(13)=4                                                      
C                                                                       
      CALL C8(N,L,M,X,A,B,P,F,CGR,CGS,Y,PAR,Q,UNCONS)                   
C                                                                       
      STOP                                                              
      END                                                               
