! TECTÈPOBAHÈE METOÄOB ÈÇ ÏAKETA ÁM MOÄÈÔÈÖÈPOBAHHÛÉ METOÄ HÜÞTOHA (A8)
! BHÈMAHÈE: ÄËß OÁPAÙEHÈß MATPÈÖÛ ÈCÏOËÜÇÓETCß ÏOÄÏPOÃPAMMA DMINV. ÏOÝTOMÓ HEOÁXOÄÈMO ÏOÄKËÞ×EHÈE ÁÈÁËÈOTEKÈ SSPLIB
PROGRAM VA8
! XAPAKTEPÈCTÈKA ÏAPAMETPOB ÏPOÃPAMMÛ METOÄA
    ! N - PAÇMEPHOCTÜ ÇAÄA×È
    ! X - BEKTOP ÓÏPABËßEMÛX ÏEPEMEHHÛX
    ! A - BEKTOP ËEBÛX ÃPAHÈÖ ÈÇMEHEHÈß ÓÏPABË. ÏEPEMEHHÛX
    ! B - BEKTOP ÏPABÛX ÃPAHÈÖ ÈÇMEHEHÈß ÓÏPABË.ÏEPEMEHHÛX
    ! F - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA FUNCTION ÄËß BÛ×ÈCËEHÈß
    ! ÇHA×EHÈß KPÈTEPÈß
    ! GRAD - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA SUBROUTINE ÄËß
    ! BÛ×ÈCËEHÈß ÃPAÄÈEHTA  ( ÈCÏOËÜÇÓETCß CBOß
    ! BCTPOEHHAß ÏPOÃPAMMA BÛ×ÈCËEHÈß ÃPAÄÈEHTA )
    ! AGS - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA SUBROUTINE ÄËß PAC×ETA
    ! MATPÈÖÛ BTOPÛX ÏPOÈÇBOÄHÛX (HE ÈCÏOËÜÇÓETCß )
    ! Y - ÇHA×EHÈE KPÈTEPÈß
    ! G1 - BEKTOP ÇHA×EHÈÉ ÏPOÈÇBOÄHÛX ÔÓHKÖÈÈ F
    ! Q - ÏAPAMETP C ÔÈKCÈPOBAHHÛM ÇHA×EHÈEM ( = 0)
    ! PAR - BEKTOP ÏAPAMETPOB METOÄA
    ! FNLP - ÔÈKCÈPOBAHHOE ÈMß ÏOÄÏPOÃPAMMÛ
! OÏÈCAHÈE ÏAPAMETPOB ÏPOÃPAMMÛ METOÄA
    ! OÏÈCAHÈE OÁÙÈX OÁËACTEÉ METOÄA
    COMMON /A81/  HES
    COMMON /A82/  P1
    COMMON /A83/  P2
    COMMON /A84/  XV
    COMMON /A85/  FV
    COMMON /A86/  XT
    COMMON /A87/  TR
    COMMON /A88/  L1
    COMMON /A829/ M1
    ! PAÇMEPHOCTÜ MACCÈBOB X,A,B,G1  PABHA  N
    REAL(8),DIMENSION(2)::X,A,B,G1
    REAL(8),DIMENSION(40)::PAR
    ! PAÇMEPHOCTÜ  MACCÈBA HES  = N*N
    REAL(8),DIMENSION(2,2)::HES
    ! PAÇMEPHOCTÜ  MACCÈBA P2   = 3*N
    REAL(8),DIMENSION(3,2)::P2
    ! PAÇMEPHOCTÜ  BCEX MACCÈBOB  = N
    REAL(8),DIMENSION(2)::P1,XV,L1,M1,FV,XT,TR
    REAL(8)::F,FNLP,Y
    INTEGER::Q
    EXTERNAL F,FNLP,GRAD,AGS
! ÈCXOÄHÛE ÄAHHÛE ÇAÄA×È
    ! PAÇMEPHOCTÜ ÇAÄA×È
    N=2
    ! HA×AËÜHAß TO×KA
    X(1)=  1.2
    X(2)=  1.0
! ÇAÄAHÈE ÏAPAMETPOB METOÄA
    Q=0
    PAR(Q+1)=0.00001 ! TO×HOCTÜ PEØEHÈß ÇAÄA×È ÏO HOPME ÃPAÄÈEHTA
    PAR(Q+2)=25      ! MAKCÈMAËÜHOE ×ÈCËO ØAÃOB,KOTOPOE MOÆHO CÄEËATÜ
    PAR(Q+3)=0       ! BÛXOÄHOÉ ÏAPAMETP
    PAR(Q+4)=0       ! ÏPÈÇHAK HA×AËÜHOÉ OÏPEÄEËEHHOCTÈ ÏAPAMETPOB Y È G1 ( 0,1 ÈËÈ 2 )
    PAR(Q+5)=0.001   ! HA×AËÜHÛÉ ØAÃ CÏÓCKA
    PAR(Q+6)=0.45    ! MAÆOPAHTA ÃOËÄCTEÉHA
    PAR(Q+7)=0.00001 ! ØAÃ KOHE×HO-PAÇHOCTHOÉ AÏÏPOKCÈMAÖÈÈ ÏPOÈÇBOÄHÛX
    PAR(Q+8)=1       ! ×ÈCËO ÓÄA×HÛX ØAÃOB,×EPEÇ KOTOPOE  CËEÄÓET BÛBOÄÈTÜ ÈHÔOPMAÖÈÞ
    PAR(Q+9)=3       ! CTEÏEHÜ ÏOÄPOÁHOCTÈ BÛBOÄÈMOÉ ÈHÔOPMAÖÈÈ ( OT 0 ÄO 3 )
! ÂÛÇÎÂ ÌÅÒÎÄÀ
    CALL A8 (N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
END PROGRAM VA8