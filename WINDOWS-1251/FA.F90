FUNCTION F(X,FNLP)
! оPхMEP оOдцOTOBKх тсHKжхх дкъ гAдAвх аEгсCкOBHOи MхHхMхгAжхх
!                ( тсмйжхъ пнгемапнйю )
    COMMON /C/ NF
    REAL(8), DIMENSION(2)::X
    REAL(8)::F
    INTEGER::NF
! CвETвхK BшвхCкEHхи тсHKжхх
    NF=NF+1
! PACвET MхHхMхгхPсEMOи тсHKжхх
    F=100 * (X(2) - X(1)**2 )**2 + (1 - X(1))**2
    RETURN
END FUNCTION F
