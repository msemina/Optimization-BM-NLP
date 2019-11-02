! ÈCÏOËÜÇOBAHÈE METOÄOB ÈÇ ÏAKETA OÏTÈMÈÇAÖÈÈ METOÄ ÏÎÈÑÊÀ ÄÎÏÓÑÒÈÌÛÕ ÒÎ×ÅÊ ( BHYTP )
PROGRAM VBNYTP
! XAPAKTEPÈCTÈKA ÏAPAMETPOB ÏPOÃPAMMÛ METOÄA
    ! N - PAÇMEPHOCTÜ BEKTOPA ÓÏPABËßEMÛX ÏEPEMEHHÛX
    ! L - ×ÈCËO OÃPAHÈ×EHÈÉ TÈÏA PABEHCTB
    ! M - OÁÙEE ×ÈCËO OÃPAHÈ×EHÈÉ
    ! X - BEKTOP ÓÏPABËßEMÛX ÏEPEMEHHÛX
    ! A - BEKTOP ËEBÛX ÃPAHÈÖ ÈÇMEHEHÈß ÓÏPABË. ÏEPEMEHHÛX
    ! B - BEKTOP ÏPABÛX ÃPAHÈÖ ÈÇMEHEHÈß ÓÏPABË.ÏEPEMEHHÛX
    ! P - BEKTOP ÄBOÉCTBEHHÛX ÏEPEMEHHÛX
    ! F - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA SUBROUTINE ÄËß BÛ×ÈCËEHÈß ÇHA×EHÈß KPÈTEPÈß È OÃPAHÈ×EHÈÉ
    ! CGR - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA SUBROUTINE ÄËß BÛ×ÈCËEHÈß ÃPAÄÈEHTOB ÖEËEBOÉ ÔÓHKÖÈÈ È OÃPAHÈ×EHÈÉ
    ! CGS - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA SUBROUTINE ÄËß PAC×ETA MATPÈÖÛ BTOPÛX ÏPOÈÇBOÄHÛX ÖEËEBOÉ ÔÓHKÖÈÈ È OÃPAHÈ×EHÈÉ
    ! Y - BEKTOP ÇHA×EHÈÉ KPÈTEPÈß È OÃPAHÈ×EHÈÉ
    ! PAR - BEKTOP ÏAPAMETPOB METOÄA
    ! Q - ÏAPAMETP C ÔÈKCÈPOBAHHÛM ÇHA×EHÈEM ( = 20)
    ! AP3 -  ÈMß ÏOÄÏPOÃPAMMÛ METOÄA ÁEÇÓCËOBHOÉ MÈHÈMÈÇAÖÈÈ
! OÏÈCAHÈE ÏAPAMETPOB ÏPOÃPAMMÛ METOÄA
    ! OÏÈCAHÈE OÁÙÈX OÁËACTEÉ METOÄA
    COMMON /A1/    M1,N,L
    COMMON /A8/    Y11
    COMMON /A9/    Y2
    COMMON /A11/   G
    COMMON /A10/   NF
    COMMON /A13/   Y1
    COMMON /AP31/  P1
    COMMON /AP32/  P2
    COMMON /AP33/  G2
    COMMON /AP34/  XV
    COMMON /AP35/  W
    COMMON /AP36/  WN
    COMMON /AGR2/  YT
    COMMON /AGR3/  GB
    COMMON /AGES1/ G1
    COMMON /AGES2/ GBT
    INTEGER::N,M,M1,L,Q,NF
    REAL(8)::F
    ! PAÇMEPHOCTÜ MACCÈBOB X,A,B PABHA N
    REAL(8),DIMENSION(3)::X,A,B
    ! PAÇMEPHOCTÜ MACCÈBA P PABHA M
    REAL(8),DIMENSION(5)::P
    ! PAÇMEPHOCTÜ MACCÈBA Y PABHA M1=M+1
    REAL(8),DIMENSION(6)::Y
    REAL(8),DIMENSION(40)::PAR
    ! PAÇMEPHOCTÜ  MACCÈBOB G,GB,G1 = N
    REAL(8),DIMENSION(3)::G,G1,GB
    ! PAÇMEPHOCTÜ  MACCÈBA GBT  = ( N,N )
    REAL(8),DIMENSION(3,3)::GBT
    ! PAÇMEPHOCTÜ  MACCÈBOB Y11,YT,Y2,Y1  = M+1
    REAL(8),DIMENSION(6)::Y11,Y2,Y1,YT
    EXTERNAL F,CGR,CGS,AP3
! ÏEPEMEHHÛE ÄËß METOÄA ÁEÇÓCËOBHOÉ OÏTÈMÈÇAÖÈÈ (AP3)
    ! PAÇMEPHOCTÜ MACCÈBOB P1,P2,G2,XV,W,WN = M
    REAL(8),DIMENSION(6)::P1,P2,G2,XV
    INTEGER,DIMENSION(6)::W,WN
! 
    NF=0
    Q=20
! ÈCXOÄHÛE ÄAHHÛE ÇAÄA×È
    ! PAÇMEPHOCTÜ ÇAÄA×È
    M1=4
    M=M1-1
    L=0
    N=2
    ! HA×AËÜHAß TO×KA
    X(1)=-10.
    X(2)= -10.
    ! ËEBÛE ÃPAHÈÖÛ ÓÏPABË. ÏEPEMEHHÛX ÏO KAÆÄOÉ KOOPÄÈHATE
    A(1)= -100000.
    A(2)= -100000.
    A(3)= -100000.
    ! ÏPABÛE ÃPAHÈÖÛ ÓÏPABË. ÏEPEMEHHÛX ÏO KAÆÄOÉ KOOPÄÈHATE
    B(1)=100000.
    B(2)=100000.
    B(3)=100000.
! ÇHA×EHÈß ÄBOÉCTBEHHÛX ÏEPEMEHHÛX
    P(1)=1.D0
    P(2)=1.D0
    P(3)=0.D0
    P(4)=0.D0
    P(5)=0.D0
! ÇAÄAHÈE ÏAPAMETPOB METOÄA
    PAR(1)=0.0001       ! TO×HOCTÜ PEØEHÈß ÇAÄA×È
    PAR(2)=6.           ! MAKCÈMAËÜHO BOÇMOÆHOE ×ÈCËO ÈTEPAÖÈÉ
    PAR(3)=0.           ! ÔAKTÈ×ECKÈ CÄEËAHHOE ×ÈCËO ÈTEPAÖÈÉ
    PAR(4)=1.           ! ×ÈCËO ØAÃOB,×EPEÇ KOTOPOE  CËEÄÓET BÛBOÄÈTÜ ÈHÔOPMAÖÈÞ
    PAR(5)=4.           ! CTEÏEHÜ ÏOÄPOÁHOCTÈ BÛBOÄÈMOÉ ÈHÔOPMAÖÈÈ ( OT 0 ÄO 4 )
! ÏAPAMETPÛ METOÄA COÏPßÆEHHÛX ÃPAÄÈEHTOB (AP3)
    PAR(Q+1)=0.1        ! TO×HOCTÜ PEØEHÈß ÇAÄA×È ÏO HOPME ÃPAÄÈEHTA
    PAR(Q+2)=10.        ! MAKCÈMAËÜHOE ×ÈCËO ØAÃOB, KOTOPOE MOÆHO CÄEËATÜ
    PAR(Q+3)=0.         ! BÛXOÄHOÉ ÏAPAMETP
    PAR(Q+4)=0.0001     ! HA×AËÜHÛÉ ØAÃ CÏÓCKA
    PAR(Q+5)=0.1        ! HA×AËÜHAß TO×HOCTÜ PEØEHÈß OÄHOMEPHOÉ ÇAÄA×È BÛÁOPA OÏTÈMAËÜHOÃO ØAÃA
    PAR(Q+6)=0.00000001 ! MÈHÈMAËÜHÛÉ ØAÃ CÏÓCKA
    PAR(Q+7)=10.        ! ×ÈCËO ÈTEPAÖÈÉ, ×EPEÇ KOTOPOE METOÄ "BOCCTAHABËÈBAETCß"
    PAR(Q+8)=1.         ! HOMEP BEPCÈÈ METOÄA ( = 1 ÈËÈ 2 )
    PAR(Q+9)=1.         ! ÏOPßÄOK ÄÈÔÔEPEHÖÈPOBAHÈß ( = 1 ÈËÈ 2 )
    PAR(Q+10)=0.0001    ! ØAÃ ÄÈÔÔEPEHÖÈPOBAHÈß
    PAR(Q+11)=1.        ! ×ÈCËO ÓÄA×HÛX ØAÃOB, ×EPEÇ KOTOPOE  CËEÄÓET BÛBOÄÈTÜ ÈHÔOPMAÖÈÞ
    PAR(Q+12)=0.        ! CTEÏEHÜ ÏOÄPOÁHOCTÈ BÛBOÄÈMOÉ ÈHÔOPMAÖÈÈ: OT 0 ÄO 3
! ÂÛÇÎÂ ÌÅÒÎÄÀ
    CALL BNYTP(N,L,M,X,A,B,P,F,CGR,CGS,Y,PAR,Q,AP3)
END PROGRAM VBNYTP
