! TECTÈPOBAHÈE METOÄOB ÈÇ ÏAKETA ÁM R – ÀËÃÎÐÈÒÌ ØÎÐÀ (A7)
PROGRAM VA7
! XAPAKTEPÈCTÈKA ÏAPAMETPOB ÏPOÃPAMMÛ METOÄA
    ! N - PAÇMEPHOCTÜ ÇAÄA×È
    ! X - BEKTOP ÓÏPABËßEMÛX ÏEPEMEHHÛX
    ! A - BEKTOP ËEBÛX ÃPAHÈÖ ÈÇMEHEHÈß ÓÏPABË. ÏEPEMEHHÛX
    ! B - BEKTOP ÏPABÛX ÃPAHÈÖ ÈÇMEHEHÈß ÓÏPABË.ÏEPEMEHHÛX
    ! F - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA FUNCTION ÄËß BÛ×ÈCËEHÈß ÇHA×EHÈß KPÈTEPÈß
    ! GRAD - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA SUBROUTINE ÄËß BÛ×ÈCËEHÈß ÃPAÄÈEHTA
    ! AGS - ÈMß ÏOÄÏPOÃPAMMÛ TÈÏA SUBROUTINE ÄËß PAC×ETA MATPÈÖÛ BTOPÛX ÏPOÈÇBOÄHÛX (HE ÈCÏOËÜÇÓETCß )
    ! Y - ÇHA×EHÈE KPÈTEPÈß
    ! G1 - BEKTOP ÇHA×EHÈÉ ÏPOÈÇBOÄHÛX ÔÓHKÖÈÈ F
    ! Q - ÏAPAMETP C ÔÈKCÈPOBAHHÛM ÇHA×EHÈEM ( = 0)
    ! PAR - BEKTOP ÏAPAMETPOB METOÄA
    ! FNLP - ÔÈKCÈPOBAHHOE ÈMß ÏOÄÏPOÃPAMMÛ
! OÏÈCAHÈE ÏAPAMETPOB ÏPOÃPAMMÛ METOÄA
    ! OÏÈCAHÈE OÁÙÈX OÁËACTEÉ METOÄA
    COMMON /A71/ G
    COMMON /A72/ G2
    COMMON /A73/ Z
    COMMON /A74/ BB
    REAL(8)::Y,F,FNLP
    INTEGER::Q
    ! PAÇMEPHOCTÜ MACCÈBOB X,A,B,G1  PABHA  N
    REAL(8),DIMENSION(2)::X,A,B,G1
    REAL(8),DIMENSION(40)::PAR
    ! PAÇMEPHOCTÜ  MACCÈBA BB = N*N
    REAL(8),DIMENSION(2,2)::BB
    ! PAÇMEPHOCTÜ  BCEX MACCÈBOB  = N
    REAL(8),DIMENSION(2)::G,G2,Z
    EXTERNAL F,FNLP,GRAD,AGS
! ÈCXOÄHÛE ÄAHHÛE ÇAÄA×È
    ! PAÇMEPHOCTÜ ÇAÄA×È
    N=2
    ! HA×AËÜHAß TO×KA
    X(1)=-1.0
    X(2)=-1.0
! ÇAÄAHÈE ÏAPAMETPOB METOÄA
    Q=0
    PAR(Q+1)=0.0001       ! TO×HOCTÜ PEØEHÈß ÇAÄA×È ÏO HOPME ÃPAÄÈEHTA
    PAR(Q+2)=20           ! MAKCÈMAËÜHOE ×ÈCËO ØAÃOB,KOTOPOE MOÆHO CÄEËATÜ
    PAR(Q+3)=0            ! BÛXOÄHOÉ ÏAPAMETP
    PAR(Q+4)=0.0000000001 ! TO×HOCTÜ PEØEHÈß ÇAÄA×È ÏO HOPME ÈÇMEHEHÈß X
    PAR(Q+5)=2.           ! KOÝÔÔÈÖÈEHT PACTßÆEHÈß ÏPOCTPAHCTBA
    PAR(Q+6)=0.00000001   ! ØAÃ ÄÈÔÔEPEHÖÈPOBAHÈß
    PAR(Q+7)=1            ! ÏOPßÄOK ÄÈÔÔEPEHÖÈPOBAHÈß ( = 1 ÈËÈ 2 )
    PAR(Q+8 )=1.          ! HA×AËÜHÛÉ ØAÃ CÏÓCKA (PACCTOßHÈE OT HA×AËÜHOÉ TO×KÈ ÄO TO×KÈ ÝKCTPEMÓMA)
    PAR(Q+9)=1            ! ×ÈCËO ÓÄA×HÛX ØAÃOB,×EPEÇ KOTOPOE  CËEÄÓET BÛBOÄÈTÜ ÈHÔOPMAÖÈÞ
    PAR(Q+10)=3           ! CTEÏEHÜ ÏOÄPOÁHOCTÈ BÛBOÄÈMOÉ ÈHÔOPMAÖÈÈ ( OT 0 ÄO 3 )
! ÂÛÇÎÂ ÌÅÒÎÄÀ
    CALL A7 (N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
END PROGRAM VA7
