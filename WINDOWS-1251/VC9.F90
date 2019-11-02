! ÈCÏOËÜÇOBAHÈE METOÄOB ÈÇ ÏAKETA OÏTÈMÈÇAÖÈÈ HËÏ METOÄ ÝËËÈÏÑÎÈÄÎÂ (C9)
PROGRAM VC9
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
    ! UNCONS -  ÈMß ÏOÄÏPOÃPAMMÛ METOÄA ÁEÇÓCËOBHOÉ MÈHÈMÈÇAÖÈÈ
! OÏÈCAHÈE ÏAPAMETPOB ÏPOÃPAMMÛ METOÄA
    ! OÏÈCAHÈE OÁÙÈX OÁËACTEÉ METOÄA
    COMMON /A1/  M1,N,L
    COMMON /A2/  XN
    COMMON /A3/  GN
    COMMON /A4/  G
    COMMON /A5/  G2
    COMMON /A6/  BG
    COMMON /A7/  Y1
    COMMON /A8/  Y11
    COMMON /A9/  Y12
    COMMON /A10/ NF
    REAL(8)::F
    INTEGER::N,L,M1,Q,M,NF
    ! PAÇMEPHOCTÜ MACCÈBOB X,A,B PABHA N
    REAL(8),DIMENSION(3)::X,A,B
    ! PAÇMEPHOCTÜ MACCÈBA P PABHA M
    REAL(8),DIMENSION(5)::P
    ! PAÇMEPHOCTÜ MACCÈBA Y PABHA M1=M+1
    REAL(8),DIMENSION(6)::Y
    REAL(8),DIMENSION(40)::PAR
    ! PAÇMEPHOCTÜ MACCÈBOB GN,XN,G,G2 = N
    REAl(8),DIMENSION(3)::XN,GN,G,G2
    ! PAÇMEPHOCTÜ MACCÈBOB Y1,Y12,Y11 = M+1
    REAl(8),DIMENSION(6)::Y1,Y11,Y12
    ! PAÇMEPHOCTÜ MACCÈBA BG = (N,N)
    REAl(8),DIMENSION(9)::BG
    EXTERNAL F,CGR,CGS
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
    X(1)=0.1
    X(2)=0.7
    ! ËEBÛE ÃPAHÈÖÛ ÓÏPABË. ÏEPEMEHHÛX ÏO KAÆÄOÉ KOOPÄÈHATE
    A(1)=-100000.
    A(2)=-100000.
    A(3)=-100000.
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
    ! TO×HOCTÜ PEØEHÈß ÇAÄA×È ÏO OÁÚEMÓ ÝËËÈÏCOÈÄA,
    PAR(1)=0.00000000001 ! ËOKAËÈÇÓÞÙEÃO PEØEHÈE
    PAR(2)=150           ! MAKCÈMAËÜHO BOÇMOÆHOE ×ÈCËO ÈTEPAÖÈÉ
    PAR(3)=0.            ! ÔAKTÈ×ECKÈ CÄEËAHHOE ×ÈCËO ÈTEPAÖÈÉ
    PAR(4)=1.            ! PAÄÈÓC HA×AËÜHOÃO ØAPA, COÄEPÆAÙEÃO PEØEHÈE
    PAR(5)=1.5           ! BEC ØTPAÔA
    PAR(6)=0.001         ! ÏAPAMETP ÏPEOÁPAÇOBAHÈß OÃPAHÈ×EHÈÉ TÈÏA PABEHCTBA B HEPABEHCTBA
    PAR(7)=0.00001       ! ØAÃ ×ÈCËEHHOÃO BÛ×ÈCËEHÈß ÃPAÄÈEHTA
    PAR(8)=1.            ! HOMEP PAÇHOCTHOÉ CXEMÛ ×ÈCËEHHOÃO BÛ×ÈCËEHÈß ÃPAÄÈEHTA ( = 1 ÈËÈ 2 )
    PAR(9)=1.            ! ×ÈCËO ØAÃOB, ×EPEÇ KOTOPOE  CËEÄÓET BÛBOÄÈTÜ ÈHÔOPMAÖÈÞ
    PAR(10)=4.           ! CTEÏEHÜ ÏOÄPOÁHOCTÈ BÛBOÄÈMOÉ ÈHÔOPMAÖÈÈ ( OT 0 ÄO 4 )
! ÂÛÇÎÂ ÌÅÒÎÄÀ
    CALL C9(N,L,M,X,A,B,P,F,CGR,CGS,Y,PAR,Q)
END PROGRAM VC9
