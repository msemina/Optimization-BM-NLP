C                                                                       
C              TECTˆPOBAHˆE METO„OB ˆ‡ AKETA M                        
C              METO„   ‹‹ˆCOˆ„OB     ( A9  )                          
C                                                                       
C        OˆCAHˆE APAMETPOB POƒPAMM› METO„A                           
C                                                                       
C      PA‡MEPHOCTœ MACCˆBOB X,A,B,G1  PABHA  N                          
         REAL *8 X(2),A(2),B(2),G1(2)                                   
         REAL *8 PAR(40),Y                                              
         REAL *8 F,FNLP                                                 
         EXTERNAL F,FNLP,GRAD,AGS                                       
         INTEGER Q                                                      
C                                                                       
C           OˆCAHˆE O™ˆX O‹ACTE‰ METO„A                              
C                                                                       
         COMMON /A91/ BG/A92 / G /A93 / G2/A94 / GN                     
     *          /A95 / XN /A96/ X1                                      
C       PA‡MEPHOCTœ  MACCˆBA BG   = N*N                                 
         REAL *8 BG  (2 ,2)                                             
C       PA‡MEPHOCTœ  BCEX MACCˆBOB  = N                                 
         REAL *8 G ( 2 ),G2( 2 ),GN( 2 ),XN( 2 ),X1(2)                  
C                                                                       
C           XAPAKTEPˆCTˆKA APAMETPOB POƒPAMM› METO„A                  
C                                                                       
C     N - PA‡MEPHOCTœ ‡A„A—ˆ                                            
C     X - BEKTOP “PAB‹ŸEM›X EPEMEHH›X                                 
C     A - BEKTOP ‹EB›X ƒPAHˆ– ˆ‡MEHEHˆŸ “PAB‹. EPEMEHH›X              
C     B - BEKTOP PAB›X ƒPAHˆ– ˆ‡MEHEHˆŸ “PAB‹.EPEMEHH›X              
C     F - ˆMŸ O„POƒPAMM› TˆA FUNCTION „‹Ÿ B›—ˆC‹EHˆŸ                 
C         ‡HA—EHˆŸ KPˆTEPˆŸ                                             
C     GRAD - ˆMŸ O„POƒPAMM› TˆA SUBROUTINE „‹Ÿ                       
C         B›—ˆC‹EHˆŸ ƒPA„ˆEHTA                                          
C     AGS - ˆMŸ O„POƒPAMM› TˆA SUBROUTINE „‹Ÿ PAC—ETA                
C           MATPˆ–› BTOP›X POˆ‡BO„H›X (HE ˆCO‹œ‡“ETCŸ )               
C     Y - ‡HA—EHˆE KPˆTEPˆŸ                                             
C     G1 - BEKTOP ‡HA—EHˆ‰ POˆ‡BO„H›X ”“HK–ˆˆ F                        
C     Q - APAMETP C ”ˆKCˆPOBAHH›M ‡HA—EHˆEM ( = 0)                     
C     PAR - BEKTOP APAMETPOB METO„A                                    
C     FNLP - ”ˆKCˆPOBAHHOE ˆMŸ O„POƒPAMM›                             
C                                                                       
C         ˆCXO„H›E „AHH›E ‡A„A—ˆ                                        
C                                                                       
C     PA‡MEPHOCTœ ‡A„A—ˆ                                                
         N=2                                                            
C     HA—A‹œHAŸ TO—KA                                                   
         X(1)=  1.2                                                     
         X(2)=  1.0                                                     
C                                                                       
C    ‡A„AHˆE APAMETPOB METO„A                                          
C                                                                       
         Q=0                                                            
C     TO—HOCTœ PE˜EHˆŸ ‡A„A—ˆ O HOPME ƒPA„ˆEHTA                         
         PAR(Q+1)=0.00001                                               
C     MAKCˆMA‹œHOE —ˆC‹O ˜AƒOB,KOTOPOE MO†HO C„E‹ATœ                     
         PAR(Q+2)=25                                                    
C     B›XO„HO‰ APAMETP                                                 
         PAR(Q+3)=0                                                     
C     PA„ˆ“C HA—A‹œHOƒO ˜APA,OPE„E‹Ÿž™EƒO O‹ACTœ OˆCKA                
         PAR(Q+4)=10.                                                   
C    TO—HOCTœ PE˜EHˆŸ O„HOMEPHO‰ ‡A„A—ˆ B›OPA ˜AƒA                     
         PAR(Q+5)=0.001                                                 
C    MˆHˆMA‹œHO „O“CTˆM›‰ O'EM ‹‹ˆCOˆ„A                             
         PAR(Q+6)=0.0000000000000001                                    
C    HOMEP BEPCˆˆ METO„A ( =1 „‹Ÿ B›“K‹›X ”“HK–ˆ‰ ; =2, EC‹ˆ           
C                           ”“HK–ˆŸ HEB›“K‹AŸ )                        
         PAR(Q+7)=2                                                     
C     OPŸ„OK „ˆ””EPEH–ˆPOBAHˆŸ ( = 1 ˆ‹ˆ 2 )                           
         PAR(Q+8)=1                                                     
C     ˜Aƒ „ˆ””EPEH–ˆPOBAHˆŸ                                              
         PAR(Q+9)=0.00001                                               
C     —ˆC‹O “„A—H›X ˜AƒOB,—EPE‡ KOTOPOE  C‹E„“ET B›BO„ˆTœ                
C     ˆH”OPMA–ˆž                                                        
         PAR(Q+10)=1                                                    
C     CTEEHœ O„POHOCTˆ B›BO„ˆMO‰ ˆH”OPMA–ˆˆ ( OT 0 „O 3 )            
         PAR(Q+11)=3                                                    
C                                                                       
         CALL A9 (N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)                   
         END                                                            
