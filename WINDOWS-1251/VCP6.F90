! ÈCÏOËÜÇOBAHÈE METOÄOB ÈÇ ÏAKETA OÏTÈMÈÇAÖÈÈ ÏÐßÌÎÉ ÌÅÒÎÄ ÌÎÄÈÔÈÖÈÐÎÂÀÍÍÎÉ ÔÓÍÊÖÈÈ ËÀÃÐÀÍÆÀ (CP6)
! (ÄËß ÓÄOBËETBOPEHÈß OÃPAHÈ×EHÈÉ ÈCÏOËÜÇÓETCß METOÄ ÁEÇÓCËOBHOÉ MÈHÈMÈÇAÖÈÈ AP3)
PROGRAM VCP6
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
    COMMON /A10/   NF
    COMMON /A11/   G
    COMMON /CP60/  YYT
    COMMON /CP61/  LX
    COMMON /CP62/  HX
    COMMON /CP63/  GR
    COMMON /CP64/  FG
    COMMON /CP65/  U
    COMMON /CP66/  GU
    COMMON /CP67/  A1
    COMMON /CP68/  B1
    COMMON /CP69/  XX
    COMMON /AGR1/  LX1
    COMMON /AP31/  P1
    COMMON /AP32/  P2
    COMMON /AP33/  G2
    COMMON /AP34/  XV
    COMMON /AP35/  W
    COMMON /AP36/  WN
    COMMON /CPFI2/ AA
    COMMON /CPFI3/ BB
    REAL(8)::F
    INTEGER::N,M,M1,L,Q,NF
    ! PAÇMEPHOCTÜ MACCÈBOB X,A,B PABHA N
    REAL(8),DIMENSION(2)::X,A,B
    ! PAÇMEPHOCTÜ MACCÈBA P PABHA M
    REAL(8),DIMENSION(5)::P
    ! PAÇMEPHOCTÜ MACCÈBA Y PABHA M1=M+1
    REAL(8),DIMENSION(6)::Y
    REAL(8),DIMENSION(40)::PAR
    ! PAÇMEPHOCTÜ MACCÈBOB G,GR,LX,HX,XX,AA,BB,LX1 = N
    REAL(8),DIMENSION(2)::G,LX,HX,GR,XX,AA,BB,LX1
    ! PAÇMEPHOCTÜ MACCÈBA FG = N*( M+1 )
    REAL(8),DIMENSION(18)::FG
    ! PAÇMEPHOCTÜ MACCÈBOB U,GU,A1,B1 = M
    REAL(8),DIMENSION(5)::U,GU,A1,B1
    ! PAÇMEPHOCTÜ MACCÈBOB Y11,YYT,Y2S = M+1
    REAL(8),DIMENSION(6)::YYT,Y11,Y2
    ! ÏEPEMEHHÛE ÄËß METOÄA ÁEÇÓCËOBHOÉ OÏTÈMÈÇAÖÈÈ (AP3)
    ! PAÇMEPHOCTÜ MACCÈBOB P1,P2,G2,XV,W,WN = max{N,M}
    REAL(8),DIMENSION(6)::P1,P2,G2,XV
    INTEGER,DIMENSION(6)::W,WN
    EXTERNAL F,CGR,CGS,AP3
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
    X(1)=0.3
    X(2)=0.7
    ! ËEBÛE ÃPAHÈÖÛ ÓÏPABË. ÏEPEMEHHÛX ÏO KAÆÄOÉ KOOPÄÈHATE
    A(1)=-100000.
    A(2)=-100000.
    ! ÏPABÛE ÃPAHÈÖÛ ÓÏPABË. ÏEPEMEHHÛX ÏO KAÆÄOÉ KOOPÄÈHATE
    B(1)=100000.
    B(2)=100000.
! ÇHA×EHÈß ÄBOÉCTBEHHÛX ÏEPEMEHHÛX
    P(1)=1.D0
    P(2)=1.D0
    P(3)=0.D0
    P(4)=0.D0
    P(5)=0.D0
! ÇAÄAHÈE ÏAPAMETPOB METOÄA
    PAR(1)=0.0001       ! TO×HOCTÜ PEØEHÈß ÇAÄA×È
    PAR(2)=10.          ! MAKCÈMAËÜHO BOÇMOÆHOE ×ÈCËO ÈTEPAÖÈÉ
    PAR(3)=0.           ! ÔAKTÈ×ECKÈ CÄEËAHHOE ×ÈCËO ÈTEPAÖÈÉ
    PAR(4)=1.           ! HOMEP BEPCÈÈ METOÄA ( = 1,2 ÈËÈ 3 )
    PAR(5)= 0.1         ! ÏAPAMETP PEÃÓËßPÈÇAÖÈÈ
    PAR(6)=0.1          ! BEC OÃPAHÈ×EHÈÉ B ÓCËOBÈÈ OCTAHOBA
    PAR(7)= 0.0001      ! ØAÃ B PAÇHOCTHOÉ CXEME ×ÈCËEHHOÃO BÛ×ÈCËEHÈß ÃPAÄÈEHTA
    PAR(8)=1.           ! HOMEP PAÇHOCTHOÉ CXEMÛ BÛ×ÈCËEHÈß ÃPAÄÈEHTA
    PAR(9)=1.           ! ×ÈCËO ØAÃOB, ×EPEÇ KOTOPOE  CËEÄÓET BÛBOÄÈTÜ ÈHÔOPMAÖÈÞ
    PAR(10)=4.          ! CTEÏEHÜ ÏOÄPOÁHOCTÈ BÛBOÄÈMOÉ ÈHÔOPMAÖÈÈ ( OT 0 ÄO 4 )
    ! ÏAPAMETPÛ METOÄA COÏPßÆEHHÛX ÃPAÄÈEHTOB (AP3)
    PAR(Q+1)=0.1        ! TO×HOCTÜ PEØEHÈß ÇAÄA×È ÏO HOPME ÃPAÄÈEHTA
    PAR(Q+2)=100.       ! MAKCÈMAËÜHOE ×ÈCËO ØAÃOB, KOTOPOE MOÆHO CÄEËATÜ
    PAR(Q+3)=0.         ! BÛXOÄHOÉ ÏAPAMETP
    PAR(Q+4)=0.0001     ! HA×AËÜHÛÉ ØAÃ CÏÓCKA
    PAR(Q+5)=0.1        ! HA×AËÜHAß TO×HOCTÜ PEØEHÈß OÄHOMEPHOÉ ÇAÄA×È BÛÁOPA OÏTÈMAËÜHOÃO ØAÃA
    PAR(Q+6)=0.00000001 ! MÈHÈMAËÜHÛÉ ØAÃ CÏÓCKA
    PAR(Q+7)=10.        ! ×ÈCËO ÈTEPAÖÈÉ,×EPEÇ KOTOPOE METOÄ "BOCCTAHABËÈBAETCß"
    PAR(Q+8)=1.         ! HOMEP BEPCÈÈ METOÄA ( = 1 ÈËÈ 2 )
    PAR(Q+9)=1.         ! ÏOPßÄOK ÄÈÔÔEPEHÖÈPOBAHÈß ( = 1 ÈËÈ 2 )
    PAR(Q+10)=0.0001    ! ØAÃ ÄÈÔÔEPEHÖÈPOBAHÈß
    PAR(Q+11)=1.        ! ×ÈCËO ÓÄA×HÛX ØAÃOB, ×EPEÇ KOTOPOE CËEÄÓET BÛBOÄÈTÜ ÈHÔOPMAÖÈÞ
    PAR(Q+12)=0.        ! CTEÏEHÜ ÏOÄPOÁHOCTÈ BÛBOÄÈMOÉ ÈHÔOPMAÖÈÈ: OT 0 ÄO 3
! ÂÛÇÎÂ ÌÅÒÎÄÀ
    CALL CP6(N,L,M,X,A,B,P,F,CGR,CGS,Y,PAR,Q,AP3)
END PROGRAM VCP6
