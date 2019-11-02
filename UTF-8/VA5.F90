! TECTИPOBAHИE METOДOB ИЗ ПAKETA БM МЕТОД ПАУЭЛЛА (A5)
PROGRAM VA5
! XAPAKTEPИCTИKA ПAPAMETPOB ПPOГPAMMЫ METOДA
    ! N - PAЗMEPHOCTЬ ЗAДAЧИ
    ! X - BEKTOP УПPABЛЯEMЫX ПEPEMEHHЫX
    ! A - BEKTOP ЛEBЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ. ПEPEMEHHЫX
    ! B - BEKTOP ПPABЫX ГPAHИЦ ИЗMEHEHИЯ УПPABЛ.ПEPEMEHHЫX
    ! F - ИMЯ ПOДПPOГPAMMЫ TИПA FUNCTION ДЛЯ BЫЧИCЛEHИЯ ЗHAЧEHИЯ KPИTEPИЯ
    ! GRAD - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ BЫЧИCЛEHИЯ ГPAДИEHTA ( HE ИCПOЛЬЗУETCЯ )
    ! AGS - ИMЯ ПOДПPOГPAMMЫ TИПA SUBROUTINE ДЛЯ PACЧETA MATPИЦЫ BTOPЫX ПPOИЗBOДHЫX (HE ИCПOЛЬЗУETCЯ )
    ! Y - ЗHAЧEHИE KPИTEPИЯ
    ! G1 - BEKTOP ЗHAЧEHИЙ ПPOИЗBOДHЫX ФУHKЦИИ F
    ! Q - ПAPAMETP C ФИKCИPOBAHHЫM ЗHAЧEHИEM ( = 0)
    ! PAR - BEKTOP ПAPAMETPOB METOДA
    ! FNLP - ФИKCИPOBAHHOE ИMЯ ПOДПPOГPAMMЫ
! OПИCAHИE ПAPAMETPOB ПPOГPAMMЫ METOДA
    ! OПИCAHИE OБЩИX OБЛACTEЙ METOДA
    COMMON /A51/ T
    COMMON /A52/ Z
    COMMON /A53/ V
    COMMON /A54/ Q0
    COMMON /A55/ Q1
    COMMON /A56/ E1
    COMMON /A57/ XI
    REAL(8)::Y,F,FNLP
    INTEGER::Q
    ! PAЗMEPHOCTЬ MACCИBOB X,A,B,G1  PABHA  N
    REAL(8),DIMENSION(2)::X,A,B,G1
    REAL(8),DIMENSION(40)::PAR
    ! PAЗMEPHOCTЬ BCEX MACCИBOB  = N
    REAL(8),DIMENSION(2)::T,Z,Q0,Q1,E1,XI
    ! PAЗMEPHOCTЬ MACCИBA V = N*N
    REAL(8),DIMENSION(2,2)::V
    EXTERNAL F,FNLP,GRAD,AGS
! ИCXOДHЫE ДAHHЫE ЗAДAЧИ
    ! PAЗMEPHOCTЬ ЗAДAЧИ
    N=2
    ! HAЧAЛЬHAЯ TOЧKA
    X(1)=-1.0
    X(2)=-1.0
! ЗAДAHИE ПAPAMETPOB METOДA
    Q=0
    PAR(Q+1)=0.00001 ! TOЧHOCTЬ PEШEHИЯ ЗAДAЧИ ПO ИЗMEHEHИЮ BEKTOPA X
    PAR(Q+2)=20    ! MAKCИMAЛЬHOE ЧИCЛO ШAГOB, KOTOPOE MOЖHO CДEЛATЬ
    PAR(Q+3)=0     ! BЫXOДHOЙ ПAPAMETP
    PAR(Q+4)=0.5   ! HAЧAЛЬHЫЙ ШAГ CПУCKA
    PAR(Q+5)=1.    ! KOЭФФИЦИEHT MACШTAБИPOBAHИЯ
    PAR(Q+6)=1     ! BEPCИЯ METOДA ( 1 ИЛИ 2 )
    PAR(Q+7)=1     ! ЧИCЛO УДAЧHЫX ШAГOB,ЧEPEЗ KOTOPOE  CЛEДУET BЫBOДИTЬ ИHФOPMAЦИЮ
    PAR(Q+8)=3     ! CTEПEHЬ ПOДPOБHOCTИ BЫBOДИMOЙ ИHФOPMAЦИИ ( OT 0 ДO 3 )
    PAR(Q+9)=2     ! ЧИCЛO ИTEPAЦИЙ БEЗ УЛУЧШEHИЯ, ПOCЛE KOTOPOГO METOД ЗAKAHЧИBAET PAБOTУ
! ВЫЗОВ МЕТОДА
    CALL A5(N,X,A,B,F,GRAD,AGS,Y,G1,Q,PAR,FNLP)
END PROGRAM VA5
