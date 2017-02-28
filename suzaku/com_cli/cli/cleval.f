C
C File: cleval.f
C Deacdiption: evaluate RHS
C Author: A.Shirahashi
C Date: 94/11/19
C
C History:
C     11-Feb-1997 Y.ISHISAKI, support '@symbol=?prompt'
C     17-Feb-2005 Y.ISHISAKI, move `command` & ?prompt support into clgetl.f
C     27-Feb-2005 Y.ISHISAKI, check Opt_DOLLAR
C     27-Feb-2005 Y.ISHISAKI, don't use local string, use 'output' only
C
      Subroutine CLeval( input,output,outlen )
      Implicit None
C input
      Character * (*) input
C output
      Character * (*) output
      Integer  outlen
C common
      include 'clflag.inc'
C local
      Integer  I, L
C begin
      output = input
      outlen = Min( Len(input), Len(output) )
      If ( Opt_DOLLAR.eq.0 ) Return
C ... symbol substition
      I = 1
      Do While ( I.le.outlen )
        If ( output(I:I).eq.'$' ) Then
          L = outlen - I + 1
          Call CLmacS( output(I:),L )
          outlen = Min( I + L - 1, Len(output) )
        End If
        I = I + 1
      End Do
C
      Return
      End
