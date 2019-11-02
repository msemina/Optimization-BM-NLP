! TECTÈPOBAHÈE METOÄOB ÈÇ ÏAKETA ÁM ÌÅÒÎÄ ÏÀÓÝËËÀ (A5)
PROGRAM VA5
! XAPAKTEPÈCTÈKA ÏAPAMETPOB ÏPOÃPAMMÛ METOÄA
    ! N - PAÇMEPHOCTÜ ÇAÄA×È
    ! X - BEKTOP ÓÏPABËßEMÛX ÏEPEMEHHÛX
    ! A - BEKTOP ËEBÛX ÃPAHÈÖ ÈÇMEHEHÈß ÓÏPABË. ÏEPEMEHHÛX
    ! B - BEKTOP ÏPABÛX ÃPAHÈÖ ÈÇMEHEHÈß ÓÏPABË.ÏEPEMEHHÛX
    ! F - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA FUNCTION ÄËß BÛ×ÈCËEHÈß ÇHA×EHÈß KPÈTEPÈß
    ! GRAD - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA SUBROUTINE ÄËß BÛ×ÈCËEHÈß ÃPAÄÈEHTA ( HE ÈCÏOËÜÇÓETCß )
    ! AGS - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA SUBROUTINE ÄËß PAC×ETA MATPÈÖÛ BTOPÛX ÏPOÈÇBOÄHÛX (HE ÈCÏOËÜÇÓETCß )
    ! Y - ÇHA×EHÈE KPÈTEPÈß
    ! G1 - BEKTOP ÇHA×EHÈÉ ÏPOÈÇBOÄHÛX ÔÓHKÖÈÈ F
    ! Q - ÏAPAMETP C ÔÈKCÈPOBAHHÛM ÇHA×EHÈEM ( = 0)
    ! PAR - BEKTOP ÏAPAMETPOB METOÄA
    ! FNLP - ÔÈKCÈPOBAHHOE ÈMß ÏOÄÏPOÃPAMMÛ
! OÏÈCAHÈE ÏAPAMETPOB ÏPOÃPAMMÛ METOÄA
    ! OÏÈCAHÈE OÁÙÈX OÁËACTEÉ METOÄA
    COMMON /A51/ T
    COMMON /A52/ Z
    COMMON /A53/ V
    COMMON /A54/ Q0
    COMMON /A55/ Q1
    COMMON /A56/ E1
    COMMON /A57/ XI
    REAL(8)::Y,F,FNLP
    INTEGER::Q
    ! PAÇMEPHOCTÜ MACCÈBOB X,A,B,G1  PABHA  N
    REAL(8),DIMENSION(2)::X,A,B,G1
    REAL(8),DIMENSION(40)::PAR
    ! PAÇMEPHOCTÜ BCEX MACCÈBOB  = N
    REAL(8),DIMENSION(2)::T,Z,Q0,Q1,E1,XI
    ! PAÇMEPHOCTÜ MACCÈBA V = N*N
    REAL(8),DIMENSION(2,2)::V
    EXTERNAL F,FNLP,GRAD,AGS
! ÈCXOÄHÛE ÄAHHÛE ÇAÄA×È
    ! PAÇMEPHOCTÜ ÇAÄA×È
    N=2
    ! HA×AËÜHAß TO×KA
    X(1)=-1.0
    X(2)=-1.0
! ÇAÄAHÈE ÏAPAMETPOB METOÄA
    Q=0
    PAR(Q+1)=0.00001 ! TO×HOCTÜ PEØEHÈß ÇAÄA×È ÏO ÈÇMEHEHÈÞ BEKTOPA X
    PAR(Q+2)=20    ! MAKCÈMAËÜHOE ×ÈCËO ØAÃOB, KOTOPOE MOÆHO CÄEËATÜ
    PAR(Q+3)=0     ! BÛXOÄHOÉ ÏAPAMETP
    PAR(Q+4)=0.5   ! HA×AËÜHÛÉ ØAÃ CÏÓCKA
    PAR(Q+5)=1.    ! KOÝÔÔÈÖÈEHT MACØTAÁÈPOBAHÈß
    PAR(Q+6)=1     ! BEPCÈß METOÄA ( 1 ÈËÈ 2 )
    PAR(Q+7)=1     ! ×ÈCËO ÓÄA×HÛX ØAÃOB,×EPEÇ KOTOPOE  CËEÄÓET BÛBOÄÈTÜ ÈHÔOPMAÖÈÞ
    PAR(Q+8)=3     ! CTEÏEHÜ ÏOÄPOÁHOCTÈ BÛBOÄÈMOÉ ÈHÔOPMAÖÈÈ ( OT 0 ÄO 3 )
    PAR(Q+9)=2     ! ×ÈCËO ÈTEPAÖÈÉ ÁEÇ ÓËÓ×ØEHÈß, ÏOCËE KOTOPOÃO METOÄ ÇAKAH×ÈBAET PAÁOTÓ
! ÂÛÇÎÂ ÌÅÒÎÄÀ
    CALL A5(N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
END PROGRAM VA5
