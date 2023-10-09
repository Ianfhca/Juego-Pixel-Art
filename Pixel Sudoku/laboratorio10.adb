WITH Ada.Text_IO, Ada.Integer_Text_IO, Nt_Console, Funciones_Aux;
USE Ada.Text_IO, Ada.Integer_Text_IO, Nt_Console, Funciones_Aux;

package body Laboratorio10 is

   ----------------
   -- Es_Lateral -- |CORRECTO|
   ----------------

   FUNCTION Es_Lateral (Img: IN T_Imagen; Fil, Col: IN Positive) RETURN Boolean IS
      Lateral: boolean:= false;
   BEGIN

      IF (Fil = Img'First(1) OR Fil = Img'Last(1)) THEN
         IF (Col /= Img'First(2) AND Col /= Img'Last(2)) THEN
            Lateral:= True;
         END IF;
      ELSE
         IF (Col = Img'First(2) OR Col = Img'Last(2)) THEN
            Lateral:= True;
         END IF;
      END IF;

      RETURN Lateral;

   END Es_Lateral;

   ----------------
   -- Es_Esquina -- |CORRECTO|
   ----------------

   FUNCTION Es_Esquina (Img:IN T_Imagen; Fil,Col:IN Positive) RETURN Boolean IS
      Esquina: Boolean:= False;
   BEGIN

      IF (Fil = Img'First(1) OR Fil = Img'Last(1)) THEN
         IF (Col = Img'First(2) OR Col = Img'Last(2)) THEN
            Esquina:= True;
         END IF;
      END IF;

      RETURN Esquina;

   END Es_Esquina;

   -----------------
   -- Es_Interior -- |CORRECTO|
   -----------------

   FUNCTION Es_Interior (Img: IN T_Imagen; Fil,Col: IN Positive) RETURN Boolean IS
      Interior: Boolean:= False;
   BEGIN

      IF (Es_Lateral(Img, Fil, Col) = False AND Es_Esquina(Img, Fil, Col) = False) THEN
         Interior:= True;
      END IF;

      RETURN Interior;

   END Es_Interior;

   ------------------
   -- Imagen_vacia -- |CORRECTO|
   ------------------

   FUNCTION Imagen_Vacia (Filas, Columnas: IN Integer) RETURN T_Imagen IS
      Imagen: T_Imagen(1..Filas, 1..Columnas);
   BEGIN

      IF (Filas > 0 AND Columnas > 0) THEN
         FOR I IN Imagen'First(1)..Imagen'Last(1) LOOP
            FOR J IN Imagen'First(2)..Imagen'Last(2) LOOP
               Imagen(I, J):= Duda;
            END LOOP;
         END LOOP;
      END IF;

      RETURN Imagen;

   END Imagen_Vacia;

   -------------
   -- Mostrar -- |CORRECTO|
   -------------

   PROCEDURE Mostrar (Img: IN T_Imagen) IS

   BEGIN

      FOR I IN Img'First(1)..Img'Last(1) LOOP
         FOR J IN Img'First(2)..Img'Last(2) LOOP
            Put(Img(I,J)'Image); Put(" ");
            END LOOP;
            New_Line(1);
      END LOOP;

   END Mostrar;

   --------------------
   -- Contar_cuadros -- |CORRECTO|
   --------------------

   PROCEDURE Contar_Cuadros (Img: IN T_Imagen; Fil, Col: IN Integer; Contador: OUT T_Contador) IS

   BEGIN

      Contador(Duda):= 0;
      Contador(Blanco):=0;
      Contador(Negro):=0;

      FOR I IN Fil-1..Fil+1 LOOP
         FOR J IN Col-1..Col+1 LOOP
            IF (I >= Img'First(1) AND I <= Img'Last(1)) THEN
               IF (J >= Img'First(2) AND J <= Img'Last(2)) THEN
                  Contador(Img(I, J)):= Contador(Img(I, J))+1;
               END IF;
            END IF;
         END LOOP;
      END LOOP;

   END Contar_Cuadros;

   --------------
   -- Completa -- |CORRECTO|
   --------------

   FUNCTION Completa (Img: IN T_Imagen) RETURN Boolean IS
      Cmpl: Boolean:= True;
      I,J: Integer;
   BEGIN
      I:= Img'First(1);
      J:= Img'First(2);

      WHILE (Cmpl AND I <= Img'Last(1)) LOOP
         WHILE (Cmpl AND J <= Img'Last(2)) LOOP
            IF (Img(I, J) = Duda) THEN
               Cmpl:= False;
            END IF;
            J:= J+1;
         END LOOP;
         J:= Img'First(2);
         I:= I+1;
      END LOOP;

      RETURN Cmpl;

   END Completa;

   --------------
   -- Colorear -- |CORRECTO|
   --------------

   PROCEDURE Colorear (Img: IN OUT T_Imagen; P: IN T_Pista) IS -- FALTA
      Contador: T_Contador;
      Color: T_Casilla:= Duda;
   BEGIN

      Contar_Cuadros(Img, P.Fil, P.Col, Contador);

      IF (P.Valor - Contador(Blanco) = Contador(Duda)) THEN
         Color:= Blanco;
      ELSIF (Contador(Blanco) = P.Valor) THEN
         Color:= Negro;
      END IF;

      FOR I IN P.Fil-1..P.Fil+1 LOOP
         FOR J IN P.Col-1..P.Col+1 LOOP
            IF (I >= Img'First(1) AND I <= Img'Last(1) AND J >= Img'First(2) AND J <= Img'Last(2)) THEN
               IF (Img(I, J) = Duda) THEN
                  Img(I, J):= Color;
               END IF;
            END IF;
         END LOOP;
      END LOOP;

   END Colorear;

   -------------
   -- Mostrar -- |CORRECTO|
   -------------

   PROCEDURE Mostrar (L: IN T_Lista_E_Pistas) IS

   BEGIN

      FOR I IN L.Rest'First..L.Cont LOOP
         Put(L.Rest(I).Valor, width => 1); Put(", ");
      END LOOP;

      New_Line(2);

   END Mostrar;

   ------------
   -- Anadir -- |CORRECTO|
   ------------

   PROCEDURE Anadir (L: IN OUT T_Lista_E_Pistas; P: IN T_Pista) IS

   BEGIN

      L.cont:= l.cont+1;
      L.Rest(L.Rest'Last):= P;

      --  Generated stub: replace with real body!
--      pragma Compile_Time_Warning (Standard.True, "Anadir unimplemented");
--      raise Program_Error with "Unimplemented procedure Anadir";
   END Anadir;

   ------------
   -- Borrar -- |CORRECTO|
   ------------

   PROCEDURE Borrar (L: IN OUT T_Lista_E_Pistas; P: IN T_Pista) IS -- REVISAR
      Encontrado: Boolean:= False;
      I: Integer:= L.Rest'First;
   BEGIN

      WHILE (Encontrado = False AND I <= L.Cont) LOOP
         IF (L.Rest(I) = P) THEN
            L.Rest(I..L.Cont-1):= L.Rest(I+1..L.Cont);
            L.Cont:= L.Cont-1;
            Encontrado:= True;
         END IF;
         I:= I+1;
      END LOOP;

   END Borrar;

   ------------------
   -- Buscar_Pista -- |NO SE USA|
   ------------------

   PROCEDURE Buscar_Pista (Lp: IN T_Lista_E_Pistas; Img: IN T_Imagen; P: OUT T_Pista) IS

   BEGIN

      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Buscar_Pista unimplemented");
      raise Program_Error with "Unimplemented procedure Buscar_Pista";
   END Buscar_Pista;

   --------------
   -- Longitud -- |CORRECTO|
   --------------

   FUNCTION Longitud (L:IN T_Lista_D_Pistas) RETURN Natural IS
      L2: T_Lista_D_Pistas:= L;
      Cont: Natural:= 0;
   BEGIN

      WHILE (L2 /= NULL) LOOP
         Cont:= Cont+1;
         L2:= L2.Sig;
      END LOOP;

      RETURN Cont;

   END Longitud;

   ------------
   -- Anadir -- |CORRECTO|
   ------------

   PROCEDURE Anadir (L: IN OUT T_Lista_D_Pistas; P: IN T_Pista) IS
      L2: T_Lista_D_Pistas:= L;
   BEGIN
      IF (L2 /= NULL) THEN
         WHILE (L2.Sig /= NULL) LOOP
            L2:= L2.Sig;
         END LOOP;
         L2.Sig:= NEW T_Nodo_Pista'(P, NULL);
      ELSE
         L:= NEW T_Nodo_Pista'(P, NULL);
      END IF;

   END Anadir;

   ----------------
   -- Concatenar -- |NO SE USA|
   ----------------

   PROCEDURE Concatenar (L1,L2: IN OUT T_Lista_D_Pistas) IS

   BEGIN

      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Concatenar unimplemented");
      raise Program_Error with "Unimplemented procedure Concatenar";
   END Concatenar;

   -------------
   -- Mostrar -- |CORRECTO|
   -------------

   PROCEDURE Mostrar (L: IN T_Lista_D_Pistas) IS
      L2: T_Lista_D_Pistas:= L;
   BEGIN

      WHILE (L2 /= NULL) LOOP -- Se puede usar 'L2 /= NULL AND THEN' antes de la primera condicion del while
         Put(L2.Pista.Valor);
         L2:=L2.Sig;
      END LOOP;

   END Mostrar;

   -------------------
   -- Iniciar_Juego -- |CORRECTO|
   -------------------

   PROCEDURE Iniciar_Juego (Ruta: IN String; Filas, Columnas: OUT Integer; LP: OUT T_Lista_E_Pistas) IS
      Fichero: File_Type;
      I, J: Integer:= 0;
      Actual: Character:= ' ';
   BEGIN
--ABRIR EL FICHERO Y COGER LAS FILAS Y COLUMNAS--
      Open(Fichero, In_File, Ruta & ".txt");
      Get(Fichero, Filas);
      Get(Fichero, Columnas);
      Skip_Line(Fichero);
      LP.Cont:=0;
-------------------------------------------------
--COGER DATOS DEL FICHERO HASTA QUE SE ACABE--
      WHILE (NOT End_Of_File(Fichero)) LOOP
         I:=I+1;
      --COGER DATOS DEL FICHERO HASTA EL SALTO DE LINEA--
         WHILE (NOT End_Of_Line(Fichero)) LOOP
            Get(Fichero, Actual);
            J:=J+1;
            IF (Actual /= '.') THEN
               LP.Cont:= LP.Cont+1;
               LP.Rest(LP.Cont).Fil:=I;
               LP.Rest(LP.Cont).Col:=J;
               LP.Rest(LP.Cont).Valor:=Integer'Value((1 => Actual));
            END IF;
         END LOOP;
      ----------------------------------------------------
         Skip_Line(Fichero);
         J:=0;
      END LOOP;
------------------------------------------------
      Close(Fichero); --CERRAR FICHERO--
      New_Line;

   END Iniciar_Juego;

   -------------------
   -- Guardar_Juego --
   -------------------

   PROCEDURE Guardar_Juego (Filename: IN String; Fils,Cols:  IN Integer; LP:  IN T_Lista_E_Pistas; LS: IN T_Lista_D_Pistas) IS
      Fichero: File_Type;
      Espacio: Constant Integer:= 1;
      L2: T_Lista_D_Pistas:= LS;
      Cont: Integer:= 0;
   BEGIN

      Create(Fichero, Out_File, Filename & ".game");
      Put(Fichero, Fils, Espacio); Put(Fichero, " ");
      Put(Fichero, Cols, Espacio);
      New_Line(Fichero);
      Put(Fichero, LP.Cont, Espacio);
      New_Line(Fichero);
      FOR I IN LP.Rest'First..LP.Cont LOOP
         Put(Fichero, LP.Rest(I).Fil, Espacio); Put(Fichero, " ");
         Put(Fichero, LP.Rest(I).Col, Espacio); Put(Fichero, " ");
         Put(Fichero, LP.Rest(I).Valor, Espacio); IF (I<Lp.Cont) THEN Put(Fichero, " "); END IF;
      END LOOP;
      New_Line(Fichero);
      New_Line(Fichero);
      Put(Fichero, Longitud(L2), Espacio);
      New_Line(Fichero);
      IF (L2/= NULL) THEN
         WHILE (L2 /= NULL) LOOP
            Cont:= Cont+1;
            Put(Fichero, L2.Pista.Fil, Espacio); Put(Fichero, " ");
            Put(Fichero, L2.Pista.Col, Espacio); Put(Fichero, " ");
            Put(Fichero, L2.Pista.Valor, Espacio); IF(Cont /= Longitud(LS)) THEN Put(Fichero, " "); END IF;
            L2:=L2.Sig;
         END LOOP;
      END IF;

      Close(Fichero);

   END Guardar_Juego;

   --------------------
   -- Reanudar_Juego --
   --------------------

   PROCEDURE Reanudar_Juego (Filename: IN String; Filas, Columnas:  OUT Integer; LP: OUT T_Lista_E_Pistas; LS: OUT T_Lista_D_Pistas) IS
      Fichero: File_Type;
      I: Integer:= LP.Rest'First;
      Solucionadas: Integer:= 0;
      P: T_Pista;
   BEGIN

      Open(Fichero, In_File, Filename & ".game");
      Get(Fichero, Filas);
      Get(Fichero, Columnas);
      Skip_Line(Fichero);
      Get(Fichero, LP.Cont);
      Skip_Line(Fichero);
      WHILE (NOT End_Of_Line(Fichero)) LOOP
         Get(Fichero, LP.Rest(I).Fil);
         Get(Fichero, LP.Rest(I).Col);
         Get(Fichero, LP.Rest(I).Valor);
         I:=I+1;
      END LOOP;
      Skip_Line(Fichero);
      Get(Fichero, Solucionadas);
      Skip_Line(Fichero);
      WHILE (NOT End_Of_Line(Fichero)) LOOP
         Get(Fichero, P.Fil);
         Get(Fichero, P.Col);
         Get(Fichero, P.Valor);
         Anadir(LS, P);
      END LOOP;

      Close(Fichero);

   END Reanudar_Juego;

   -------------------------
   -- Es_Posible_Resolver -- |CORRECTO|
   -------------------------

   FUNCTION Es_Posible_Resolver (Img: IN T_Imagen; P: IN T_Pista) RETURN Boolean IS
      Contador: T_Contador;
      Resoluble: Boolean:= False;
   BEGIN

      Contar_Cuadros(Img, P.Fil, P.Col, Contador);

      IF (P.Valor - Contador(Blanco) = Contador(Duda) OR Contador(Blanco) = P.Valor) THEN
         Resoluble:= True;
      END IF;

      RETURN Resoluble;

   END Es_Posible_Resolver;

   -------------------
   -- Obtener_Pista -- |NO SE USA|
   -------------------

   PROCEDURE Obtener_Pista (Filas,Columnas: IN Integer; Lp: IN T_Lista_E_Pistas; P: OUT T_Pista) IS

   BEGIN

      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Obtener_Pista unimplemented");
      raise Program_Error with "Unimplemented procedure Obtener_Pista";
   END Obtener_Pista;

   --------------
   -- Resolver -- |NO SE USA|
   --------------

   FUNCTION Resolver (Filas, Columnas: IN Integer; Lp: IN T_Lista_E_Pistas) RETURN T_Imagen IS

   BEGIN

      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Resolver unimplemented");
      raise Program_Error with "Unimplemented function Resolver";
      return Resolver (Filas, Columnas, Lp);
   END Resolver;

   ------------
   -- Fase_1 -- |CORRECTO|
   ------------

   PROCEDURE Fase_1 (Filas, Columnas: IN Integer; LP: IN OUT T_Lista_E_Pistas; Sol: OUT T_Lista_D_Pistas) IS
      Img: T_Imagen:= Imagen_Vacia(Filas, Columnas);
      Encontrado, Posible: Boolean;
      Cont: Integer:= 0;
      Siguiente: Character;
      I: Integer:= LP.Rest'First;
   BEGIN

      Posible:= True;

      WHILE (NOT Completa(Img) AND Posible) LOOP
         Imprimir(Img, LP, Sol);
         Put("Continuar resolviendo? (S/N) ");
         Get(Siguiente);
         Encontrado:= False;
         I:= LP.Rest'First;
         IF (Siguiente = 's' OR Siguiente = 'S') THEN
            Cont:= 0;
            Posible:= True;
            FOR I IN LP.Rest'First..LP.Cont LOOP
               IF (Es_Posible_Resolver(Img, LP.Rest(I))) THEN
                     Cont:=Cont+1;
               END IF;
            END LOOP;

            IF (Cont < 1) THEN
               Posible:= False;
            END IF;

            WHILE (Encontrado = False AND I <= Lp.Cont AND Posible) LOOP
               IF (Es_Posible_Resolver(Img, LP.Rest(I))) THEN
                  Colorear(Img, LP.Rest(I));
                  Anadir(Sol, LP.Rest(I));
                  Borrar(LP, LP.Rest(I));
                  Encontrado:= True;
               ELSE
                  I:=I+1;
               END IF;
            END LOOP;

         ELSIF (Siguiente = 'n' OR Siguiente = 'N') THEN
            Put_Line("Gracias por jugar!");
            RETURN;
         END IF;
      END LOOP;

      Imprimir(Img, LP, Sol);

      IF (Completa(Img)) THEN
         Put("JUEGO RESUELTO!");
      ELSE
         Put("EL JUEGO ES IRRESOLUBLE");
      END IF;
      LP.Cont:= 0;
      Sol:= Null;
      New_Line(2);

   END Fase_1;

   ------------
   -- Fase_2 -- |CORRECTO|
   ------------

   PROCEDURE Fase_2 (Filas, Columnas: IN Integer; LP: IN OUT T_Lista_E_Pistas; Sol: IN OUT T_Lista_D_Pistas) IS
      Img: T_Imagen:= Imagen_Vacia(Filas, Columnas);
      Encontrado: Boolean;
      F, C: Integer:= Integer'First;
      P: T_Pista;
      LS: T_Lista_D_Pistas:= Sol;
      I: Integer:= LP.Rest'First;
      Cont: Integer:= 0;
      Posible: Boolean:= True;
   BEGIN

      WHILE (LS  /= NULL) LOOP
         Colorear(Img, LS.Pista);
         LS:= LS.Sig;
      END LOOP;

      WHILE (NOT Completa(Img) AND Posible) LOOP
         Imprimir(Img, LP, Sol);
         Put(" # (-1) Para salir del juego ");
         New_Line;
         I:= LP.Rest'First;
         Encontrado:= False;
         Cont:= 0;
         Posible:= True;
         FOR I IN LP.Rest'First..LP.Cont LOOP
            IF (Es_Posible_Resolver(Img, LP.Rest(I))) THEN
                  Cont:=Cont+1;
            END IF;
         END LOOP;

         IF (Cont < 1) THEN
            Posible:= False;
         END IF;
         WHILE (NOT Encontrado AND Posible) LOOP
            Put("Introduce la FILA: ");
            Get(F);
            IF (F = -1) THEN
               Put_Line("Gracias por jugar! Vuelve pronto!");
               RETURN;
            ELSIF (F >= Img'First(1) AND F <= Img'Last(1)) THEN
               Encontrado:= True;
            ELSE
               Set_Foreground(Red);
               Put_Line(" **La FILA esta fuera de rango** ");
               Set_Foreground(Gray);
            END IF;
         END LOOP;

         Encontrado:= False;
         WHILE (NOT Encontrado) LOOP
            Put("Introduce la COLUMNA: ");
            Get(C);
            IF (C = -1) THEN
               Put_Line("Gracias por jugar! Vuelve pronto!");
               RETURN;
            ELSIF (C >= Img'First(2) AND C <= Img'Last(2)) THEN
              Encontrado:= True;
            ELSE
               Set_Foreground(Red);
               Put_Line(" **La COLUMNA esta fuera de rango** ");
               Set_Foreground(Gray);
            END IF;
         END LOOP;

         Encontrado:= False;
         WHILE (NOT Encontrado AND I <= Lp.Cont) LOOP
            IF (LP.Rest(I).Fil = F AND LP.Rest(I).Col = C) THEN
               P.Fil:= F;
               P.Col:= C;
               P.Valor:= LP.Rest(I).Valor;
               Encontrado:=True;
            ELSE
               I:=I+1;
            END IF;
         END LOOP;

         IF (Encontrado) THEN
            IF (Es_Posible_Resolver(Img, P)) THEN
               Colorear(Img, P);
               Anadir(Sol, P);
               Borrar(LP, P);
               Encontrado:= True;
            ELSE
               Set_Foreground(Yellow);
               Put_Line(" **Esa PISTA no se puede resolver aun** ");
               Set_Foreground(Gray);
            END IF;
         ELSE
            Set_Foreground(Red);
            Put_Line(" **La COLUMNA/FILA introducidas no corresponden a ninguna pista** ");
            Set_Foreground(Gray);
         END IF;
      END LOOP;

      Imprimir(Img, LP, Sol);

      IF (Completa(Img)) THEN
         Set_Foreground(White);
         Put("ENHORABUENA HAS RESUELTO EL JUEGO!");
         Set_Foreground(Gray);
      ELSE
         Put("EL JUEGO ES IRRESOLUBLE");
      END IF;
      LP.Cont:= 0;
      Sol:= Null;
      New_Line(2);

   END Fase_2;

END Laboratorio10;
