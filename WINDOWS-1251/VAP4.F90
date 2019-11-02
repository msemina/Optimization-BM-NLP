! TECTÈPOBAHÈE METOÄOB ÈÇ ÏAKETA ÁM METOÄ ÕÓÊÀ-ÄÆÈÂÑÀ (AP4)
PROGRAM VAP4
! XAPAKTEPÈCTÈKA ÏAPAMETPOB ÏPOÃPAMMÛ METOÄA
    ! N - PAÇMEPHOCTÜ ÇAÄA×È
    ! X - BEKTOP ÓÏPABËßEMÛX ÏEPEMEHHÛX
    ! A - BEKTOP ËEBÛX ÃPAHÈÖ ÈÇMEHEHÈß ÓÏPABË. ÏEPEMEHHÛX
    ! B - BEKTOP ÏPABÛX ÃPAHÈÖ ÈÇMEHEHÈß ÓÏPABË. ÏEPEMEHHÛX
    ! F - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA FUNCTION ÄËß BÛ×ÈCËEHÈß ÇHA×EHÈß KPÈTEPÈß
    ! GRAD - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA SUBROUTINE ÄËß BÛ×ÈCËEHÈß ÃPAÄÈEHTA (HE ÈCÏOËÜÇÓETCß)
    ! AGS - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA SUBROUTINE ÄËß PAC×ETA MATPÈÖÛ BTOPÛX ÏPOÈÇBOÄHÛX (HE ÈCÏOËÜÇÓETCß)
    ! Y - ÇHA×EHÈE KPÈTEPÈß
    ! G1 - BEKTOP ÇHA×EHÈÉ ÏPOÈÇBOÄHÛX ÔÓHKÖÈÈ F
    ! Q - ÏAPAMETP C ÔÈKCÈPOBAHHÛM ÇHA×EHÈEM ( = 0)
    ! PAR - BEKTOP ÏAPAMETPOB METOÄA
    ! FNLP - ÔÈKCÈPOBAHHOE ÈMß ÏOÄÏPOÃPAMMÛ
! OÏÈCAHÈE ÏAPAMETPOB ÏPOÃPAMMÛ METOÄA
    ! OÏÈCAHÈE OÁÙÈX OÁËACTEÉ METOÄA
    COMMON /AP41/ X1
    COMMON /AP42/ X2
    COMMON /AP43/ ST
    COMMON /AP44/ SS
    COMMON /AP45/ SF
    REAL(8)::F,FNLP,Y
    INTEGER::Q
    ! PAÇMEPHOCTÜ MACCÈBOB X,A,B,G1 PABHA N
    REAL(8),DIMENSION(2)::X,A,B,G1
    REAL(8),DIMENSION(40)::PAR
    ! PAÇMEPHOCTÜ BCEX MACCÈBOB = N
    REAL(8),DIMENSION(2)::X1,X2,ST
    INTEGER,DIMENSION(2)::SS,SF
    EXTERNAL F,FNLP,GRAD,AGS
! ÈCXOÄHÛE ÄAHHÛE ÇAÄA×È
    ! PAÇMEPHOCTÜ ÇAÄA×È
    N=2
    ! HA×AËÜHAß TO×KA
    X(1)=-1.0
    X(2)=-1.0
    ! ËEBÛE ÃPAHÈÖÛ ÓÏPABË. ÏEPEMEHHÛX ÏO KAÆÄOÉ KOOPÄÈHATE
    A(1)=-10.
    A(2)=-10.
    ! ÏPABÛE ÃPAHÈÖÛ ÓÏPABË. ÏEPEMEHHÛX ÏO KAÆÄOÉ KOOPÄÈHATE
    B(1)=10.
    B(2)=10.
! ÇAÄAHÈE ÏAPAMETPOB METOÄA
    Q=0
    PAR(Q+1)=0.00001 ! TO×HOCTÜ PEØEHÈß ÇAÄA×È
    PAR(Q+2)=100.    ! MAKCÈMAËÜHOE ×ÈCËO ØAÃOB,KOTOPOE MOÆHO CÄEËATÜ
    PAR(Q+3)=0       ! BÛXOÄHOÉ ÏAPAMETP
    PAR(Q+4)=0.01    ! HA×AËÜHÛÉ ØAÃ CÏÓCKA
    PAR(Q+5)=0.125   ! KOÝÔÔÈÖÈEHT ÓMEHÜØEHÈß ØAÃA
    PAR(Q+6)=0.00001 ! "HEOÏPEÄEËEHHOCTÜ" ÇAÄAHÈß ÃPAHÈÖ ÏAPAËËEËEÏÈÏEÄA
    PAR(Q+ 7)=1      ! ×ÈCËO ÓÄA×HÛX ØAÃOB,×EPEÇ KOTOPOE  CËEÄÓET BÛBOÄÈTÜ ÈHÔOPMAÖÈÞ
    PAR( Q+8)=3      ! CTEÏEHÜ ÏOÄPOÁHOCTÈ BÛBOÄÈMOÉ ÈHÔOPMAÖÈÈ: OT 0 ÄO 3
! ÂÛÇÎÂ ÌÅÒÎÄÀ
    CALL AP4(N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
END PROGRAM VAP4
