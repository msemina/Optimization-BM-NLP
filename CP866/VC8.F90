C                                                                       
C           ˆCO‹œ‡OBAHˆE METO„OB ˆ‡ AKETA OTˆMˆ‡A–ˆˆ H‹             
C                    METO„ HœžTOHA  ( C8 )                              
C                                                                       
C           XAPAKTEPˆCTˆKA APAMETPOB POƒPAMM› METO„A                  
C                                                                       
C     N - PA‡MEPHOCTœ BEKTOPA “PAB‹ŸEM›X EPEMEHH›X                    
C     L - —ˆC‹O OƒPAHˆ—EHˆ‰ TˆA PABEHCTB                               
C     M - O™EE —ˆC‹O OƒPAHˆ—EHˆ‰                                       
C     X - BEKTOP “PAB‹ŸEM›X EPEMEHH›X                                 
C     A - BEKTOP ‹EB›X ƒPAHˆ– ˆ‡MEHEHˆŸ “PAB‹. EPEMEHH›X              
C     B - BEKTOP PAB›X ƒPAHˆ– ˆ‡MEHEHˆŸ “PAB‹.EPEMEHH›X              
C     P - BEKTOP „BO‰CTBEHH›X EPEMEHH›X                                
C     F - ˆMŸ O„POƒPAMM› TˆA SUBROUTINE „‹Ÿ B›—ˆC‹EHˆŸ               
C         ‡HA—EHˆŸ KPˆTEPˆŸ ˆ OƒPAHˆ—EHˆ‰                               
C     CGR - ˆMŸ O„POƒPAMM› TˆA SUBROUTINE „‹Ÿ                        
C         B›—ˆC‹EHˆŸ ƒPA„ˆEHTOB –E‹EBO‰ ”“HK–ˆˆ ˆ                       
C         OƒPAHˆ—EHˆ‰                                                   
C     CGS - ˆMŸ O„POƒPAMM› TˆA SUBROUTINE „‹Ÿ PAC—ETA                
C           MATPˆ–› BTOP›X POˆ‡BO„H›X –E‹EBO‰ ”“HK–ˆˆ ˆ                
C           OƒPAHˆ—EHˆ‰                                                 
C     Y - BEKTOP ‡HA—EHˆ‰ KPˆTEPˆŸ ˆ OƒPAHˆ—EHˆ‰                        
C     PAR - BEKTOP APAMETPOB METO„A                                    
C     Q - APAMETP C ”ˆKCˆPOBAHH›M ‡HA—EHˆEM ( = 20)                    
C     UNCONS -  ˆMŸ O„POƒPAMM› METO„A E‡“C‹OBHO‰                     
C               MˆHˆMˆ‡A–ˆˆ                                             
C                                                                       
C      OˆCAHˆE APAMETPOB POƒPAMM› METO„A                             
C                                                                       
C      PA‡MEPHOCTœ MACCˆBOB X,A,B  PABHA  N                             
         REAL *8 X(2),A(2),B(2)                                         
C      PA‡MEPHOCTœ MACCˆBA  P   PABHA  M                                
         REAL *8 P(4)                                                   
C      PA‡MEPHOCTœ MACCˆBA  Y   PABHA  M1=M+1                           
         REAL *8 Y(4)                                                   
         REAL *8 PAR(40)                                                
         REAL *8 F                                                      
         COMMON /A10/NF /A1/M1,N,L                                      
         INTEGER N,L,M1,Q,M,NF                                          
         EXTERNAL F,CGR,CGS,UNCONS                                      
C                                                                       
C           OˆCAHˆE O™ˆX O‹ACTE‰ METO„A                              
C                                                                       
      COMMON/A5/FUNC /A8/Y1 /A9/Y2 /A13/Y11 /C81/ACTIV /C82/XPR         
      COMMON/C83/GR /C84/HES /C85/LZ /C86/LZZ /C87/NAPR /C88/DVOY       
      COMMON/C89/DVPR /C80/ZNFPR /C801/LRAB /C802/MRAB                  
C                                                                       
C       PA‡MEPHOCTœ  MACCˆBOB GR,XPR  = N                               
      REAL*8 XPR(2),GR(2)                                               
C       PA‡MEPHOCTœ  MACCˆBOB DVOY,DVPR = M                             
      REAL*8 DVOY(3),DVPR(3)                                            
C       PA‡MEPHOCTœ  MACCˆBOB Y1,Y2,Y11,FUNC,ZNFPR,ACTIV  = M+1         
      REAL*8 FUNC(4),Y1(4),Y2(4),Y11(4),ZNFPR(4)                        
      INTEGER ACTIV(4)                                                  
C       PA‡MEPHOCTœ  MACCˆBA HES = ( N,N )                              
      REAL*8 HES(2,2)                                                   
C       PA‡MEPHOCTœ  MACCˆBA LZZ = ( N+M,N+M )                          
      REAL*8 LZZ(7,7)                                                   
C       PA‡MEPHOCTœ  MACCˆBOB LZ,NAPR,LRAB,MRAB = ( N+M )               
      REAL*8  LZ(7), NAPR(7),LRAB(7),MRAB(7)                            
C                                                                       
      NF=0                                                              
      Q=20                                                              
C                                                                       
C                                                                       
C         ˆCXO„H›E „AHH›E ‡A„A—ˆ                                        
C                                                                       
C     PA‡MEPHOCTœ ‡A„A—ˆ                                                
      M1=4                                                              
      M=M1-1                                                            
      L=0                                                               
      N=2                                                               
C     HA—A‹œHAŸ TO—KA                                                   
         X(1)=0.1                                                       
         X(2)=0.7                                                       
         X(3)=0.2                                                       
C     ‹EB›E ƒPAHˆ–› “PAB‹. EPEMEHH›X O KA†„O‰ KOOP„ˆHATE             
         A(1)= -100000.                                                 
         A(2)= -100000.                                                 
         A(3)= -100000.                                                 
C     PAB›E ƒPAHˆ–› “PAB‹. EPEMEHH›X O KA†„O‰ KOOP„ˆHATE            
         B(1)=100000.                                                   
         B(2)=100000.                                                   
         B(3)=100000.                                                   
C                                                                       
C   ‡HA—EHˆŸ „BO‰CTBEHH›X EPEMEHH›X                                    
         P(1)=1.D0                                                      
         P(2)=1.D0                                                      
         P(3)=0.D0                                                      
         P(4)=0.D0                                                      
         P(5)=0.D0                                                      
C                                                                       
C    ‡A„AHˆE APAMETPOB METO„A                                          
C                                                                       
C     TO—HOCTœ PE˜EHˆŸ ‡A„A—ˆ O HOPME ƒPA„ˆEHTA ”“HK–ˆˆ                
C     ‹AƒPAH†A                                                          
         PAR(1)=0.0001                                                  
C     MAKCˆMA‹œHO BO‡MO†HOE —ˆC‹O ˆTEPA–ˆ‰                              
         PAR(2)=15                                                      
C     ”AKTˆ—ECKˆ C„E‹AHHOE —ˆC‹O ˆTEPA–ˆ‰                               
         PAR(3)=0                                                       
C     APAMETP B›OPA ˜AƒA „Bˆ†EHˆŸ ( MA†OPAHTA ƒO‹„CTE‰HA )            
         PAR(4)=0.8                                                     
C     APAMETP B›„E‹EHˆŸ AKTˆBH›X OƒPAHˆ—EHˆ‰                           
         PAR(5)=0.01                                                    
C     ‡HA—EHˆE,PˆCBAˆBAEMOE „BO‰CTBEHH›M EPEMEHH›M,                   
C     COOTBETCTB“ž™ˆM OƒPAHˆ—EHˆŸM TˆA HEPABEHCTBA,                    
C     HA—A‹œH›E ‡HA—EHˆŸ KOTOP›X MEHœ˜E 10** ( -18 )                    
         PAR(6)=0.1                                                     
C     MˆHˆMA‹œHOE ‡HA—EHˆE „BO‰CTBEHHO‰ EPEMEHHO‰,                     
C     Pˆ KOTOPOM OƒPAHˆ—EHˆE TˆA HEPABEHCTBA E™E                      
C     C—ˆTAETCŸ AKTˆBH›M                                                
         PAR(7)=0.1                                                     
C     ˜Aƒ —ˆC‹EHHOƒO B›—ˆC‹EHˆŸ ƒPA„ˆEHTA                               
         PAR(8)=0.0001                                                  
C     HOMEP PA‡HOCTHO‰ CXEM› —ˆC‹EHHOƒO B›—ˆC‹EHˆŸ                      
C     ƒPA„ˆEHTA ( = 1 ˆ‹ˆ 2 )                                           
         PAR(9)=2                                                       
C     ˜Aƒ —ˆC‹EHHOƒO B›—ˆC‹EHˆŸ ƒECCˆAHA                                
         PAR(10)=0.0001                                                 
C     HOMEP PA‡HOCTHO‰ CXEM› —ˆC‹EHHOƒO B›—ˆC‹EHˆŸ                      
C     ƒECCˆAHA  ( = 1,2 ˆ‹ˆ 3 )                                         
         PAR(11)=1                                                      
C     —ˆC‹O ˜AƒOB,—EPE‡ KOTOPOE  C‹E„“ET B›BO„ˆTœ                       
C     ˆH”OPMA–ˆž                                                        
         PAR(12)=1                                                      
C     CTEEHœ O„POHOCTˆ B›BO„ˆMO‰ ˆH”OPMA–ˆˆ ( OT 0 „O 4 )            
         PAR(13)=4                                                      
C                                                                       
      CALL C8(N,L,M,X,A,B,P,F,CGR,CGS,Y,PAR,Q,UNCONS)                   
C                                                                       
      STOP                                                              
      END                                                               
