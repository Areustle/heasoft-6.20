      FUNCTION funzione(x)
      real*8 x, gamma, funzione
      COMMON /funzio/ gamma
      IF ( x.LE.12.3 ) THEN
        funzione = x
      ELSE
        funzione = (0.2818*10.**(.0443*x))**gamma*x
      END IF
      RETURN
      END
