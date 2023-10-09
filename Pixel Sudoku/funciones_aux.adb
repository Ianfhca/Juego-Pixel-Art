WITH Ada.Text_IO, Ada.Integer_Text_IO, Nt_Console;
USE Ada.Text_IO, Ada.Integer_Text_IO, Nt_Console;

PACKAGE BODY Funciones_Aux IS

   --------------
   -- Imprimir -- |CORRECTO|
   --------------
   PROCEDURE Imprimir (Img: IN T_Imagen; LP: IN T_Lista_E_Pistas; LS: IN T_Lista_D_Pistas) IS
      Cont: Integer;
      Encontrado: Boolean:= False;
      LS2: T_Lista_D_Pistas:= LS;
   BEGIN

      Cont:=LP.Rest'First;
      Put("   ");
--ENUMERAR COLUMNAS--
      Set_Foreground(White);
      FOR J IN Img'First(2)..Img'Last(2) LOOP
         Put(J, Width => 1); Put(" ");
         IF (J < 10) THEN
            Put(" ");
         END IF;
      END LOOP;
      New_Line(1);
----------------------
--IMPRIMIR PISTAS CON FONDO DE COLORES--
      FOR I IN Img'First(1)..Img'Last(1) LOOP
      --ENUMERAR FILAS--
         Set_Foreground(White);
         Set_Background(Black);
         Put(I, Width => 1); Put(" ");
         IF (I < 10) THEN
            Put(" ");
         END IF;
       -----------------
         FOR J IN Img'First(2)..Img'Last(2) LOOP

            Set_Foreground(Black);

            IF (Img(I, J) = Duda) THEN
               Set_Background(Yellow);
            ELSIF (Img(I, J) = Blanco) THEN
               Set_Background(White);
            ELSE
               Set_Background(Black);
            END IF;

            IF (LP.Rest(Cont).Fil = I AND Lp.Rest(Cont).Col = J) THEN
               IF (Img(I, J) = Blanco) THEN
                  Set_Foreground(Black);
               ELSIF (Img(I, J) = Negro) THEN
                  Set_Foreground(White);
               END IF;

               Put(" "); Put(LP.Rest(Cont).Valor, Width => 1); Put(" ");
               Cont:= Cont+1;
            ELSE

               WHILE (NOT Encontrado AND LS2 /= NULL) LOOP
                  IF (LS2.Pista.Fil = I AND LS2.Pista.Col = J) THEN
                     Encontrado := True;
                  ELSE
                     LS2:=LS2.Sig;
                  END IF;
               END LOOP;
               IF (Encontrado) THEN
                  IF (Img(I, J) = Blanco) THEN
                     Set_Foreground(Black);
                  ELSIF (Img(I, J) = Negro) THEN
                     Set_Foreground(White);
                  END IF;
                  Put(" "); Put(LS2.Pista.Valor, Width => 1); Put(" ");
               ELSE
                  IF (Img(I, J) = Blanco) THEN
                     Set_Foreground(Black);
                  ELSIF (Img(I, J) = Negro) THEN
                     Set_Foreground(White);
                  END IF;
                  Put("[] ");
               END IF;
               Encontrado:= False;
               LS2:= LS;
            END IF;

         END LOOP;
         New_Line;

      END LOOP;
------------------------------------------
      Set_Foreground(White);
      Set_Background(Black);

   END Imprimir;

END Funciones_Aux;
