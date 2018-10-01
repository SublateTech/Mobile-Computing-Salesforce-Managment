IDENTIFICATION DIVISION.

PROGRAM-ID.             'DEM-AUTO'.
AUTHOR.                 Jos‚ A. Arellano M.

DATE-WRITTEN.           29/Noviembre/1995.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER.        PC.
OBJECT-COMPUTER.        t710.
SPECIAL-NAMES.
        KEY     KMEM       IS       1.        ; Muestra Cantidad Memoria
        KEY     NO         IS       2.        ; Respuesta Negativa
        KEY     YES        IS       3.        ; Respuesta Positiva
        KEY     ADJ        IS       5.        ; Ajusta luminosidad de pantalla
        KEY     KMOD       IS       6.        ; para modificar fecha y hora
        KEY     KNEXT      IS       7.        ; Muestra los Datos Restantes.
        KEY     KSTOCK     IS       7.        ; Muestra el Stock del Producto.
        KEY     KDEL       IS       8.        ; Elimina un Item del detalle
        KEY     KRELOJ     IS       9.        ; Mueestra Reloj

DATA DIVISION.

FILE SECTION.

FD  ARCHIVO-F
    recording mode is FREE
    type is 255.

FD  BUSCA-ARCH
    recording mode is FREE
    type is X'00'.

FD  BUSCA-ARCH-ALL
    recording mode is FREE
    type is 255.

FD  PROGRAMA-F                          ;ARCHIVO PARA EFECTUAR
    recording mode is FREE              ;LAS LLAMADAS A LOS
    type is PROGRAM.                    ;DISTINTOS PROGRAMAS


;ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¸
;³                        ESTRUCTURA DE ARCHIVOS                         ³
;ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾


FD IDENTIFI-F                  ; Archivo IDENTIFI.D
   RECORDING MODE IS FIXED
   TYPE IS 'D'.

01 IDENTIFI-R.
   03 IDE-VENDED            PIC 9(05).    ; C¢digo de Vendedor
   03 IDE-NOMBRE            PIC X(30).    ; Nombre de Vendedor
   03 IDE-BODEGA            PIC 9(04).    ; C¢digo de Bodega
   03 IDE-PJ-IVA            PIC 9(04).    ; Porcentaje IVA
   03 IDE-PJ-ILA            PIC 9(04).    ; Porcentaje ILA
   03 IDE-CON-IN            PIC 9(03).    ; C¢digo Condici¢n de Venta Inicial
   03 IDE-POR-IN            PIC 9(04).    ; Porcentaje Descuento Base Inicial
   03 IDE-VISITA            PIC X(01).    ; D¡a de Visita
   03 IDE-PASSWO            PIC X(04).    ; Password
   03 IDE-MAR-AU            PIC 9(01).    ; Marca Autorizada Dcto. Promocional
   03 IDE-FAC-IN            PIC 9(07).    ; N£mero de Factura Inicial
   03 IDE-FAC-FI            PIC 9(07).    ; N£mero de Factura Final
   03 IDE-FAC-EM            PIC 9(07).    ; N£mero Ultima Factura Emitida
   03 IDE-RET               PIC 9 BINARY. ; Caracter Ret de carro  X'0D'
   03 IDE-FLI               PIC 9 BINARY. ; Caracter Fin de linea  X'0A'
;  ----------------------------------
;  TOTAL                        82

FD CL-CARTE-F                  ; Archivo CL-CARTE.D = Clientes Cartera
   RECORDING MODE IS FIXED
   TYPE IS 'D'.

01 CL-CARTE-R.
   03 CL-C-LL1.
      05 CL-C-LOCAL         PIC 9(02).    ; Local
      05 CL-C-RUTTT.
         07 CL-C-RUT        PIC 9(08).    ; RUT
         07 CL-C-DIG        PIC X(01).    ; Digito

   03 CL-C-RAZON            PIC X(30).    ; Raz¢n Social
   03 CL-C-DESPAC           PIC X(30).    ; Direcci¢n Despacho
   03 CL-C-CONDIC           PIC 9(03).    ; C¢digo Condici¢n de Venta
   03 CL-C-DCT-BA           PIC 9(04).    ; Porcentaje Descuento Base
   03 CL-C-VENDED           PIC 9(05).    ; C¢digo Vendedor
   03 CL-C-VISITA           PIC X(01).    ; D¡a de Visita
   03 CL-C-CIUDAD           PIC X(13).    ; Ciudad
   03 CL-C-RET              PIC 9 BINARY. ; Caracter Ret de carro  X'0D'
   03 CL-C-FLI              PIC 9 BINARY. ; Caracter Fin de linea  X'0A'
;  ----------------------------------
;  TOTAL                       98

FD CL-NUEVO-F                  ; Archivo CL-CARTE.D = Clientes Nuevos
   RECORDING MODE IS FIXED
   TYPE IS 'D'.

01 CL-NUEVO-R.
   03 CL-N-LL1.
      05 CL-N-LOCAL         PIC 9(02).    ; Local
      05 CL-N-RUTTT.
         07 CL-N-RUT        PIC 9(08).    ; RUT
         07 CL-N-DIG        PIC X(01).    ; Digito

   03 CL-N-RAZON            PIC X(30).    ; Raz¢n Social
   03 CL-N-DESPAC           PIC X(30).    ; Direcci¢n Despacho
   03 CL-N-CONDIC           PIC 9(03).    ; C¢digo Condici¢n de Venta
   03 CL-N-DCT-BA           PIC 9(04).    ; Porcentaje Descuento Base
   03 CL-N-VENDED           PIC 9(05).    ; C¢digo Vendedor
   03 CL-N-VISITA           PIC X(01).    ; D¡a de Visita
   03 CL-N-CIUDAD           PIC X(13).    ; Ciudad
   03 CL-N-RET              PIC 9 BINARY. ; Caracter Ret de carro  X'0D'
   03 CL-N-FLI              PIC 9 BINARY. ; Caracter Fin de linea  X'0A'
;  ----------------------------------
;  TOTAL                       98

FD CL-RUTA-F                  ; Archivo CL-RUTA.D = Clientes Ruta
   RECORDING MODE IS FIXED
   TYPE IS 'D'.

01 CL-RUTA-R.
   03 CL-R-LL1.
      05 CL-R-LOCAL         PIC 9(02).    ; Local
      05 CL-R-RUTTT.
         07 CL-R-RUT        PIC 9(08).    ; RUT
         07 CL-R-DIG        PIC X(01).    ; Digito

   03 CL-R-RAZON            PIC X(30).    ; Raz¢n Social
   03 CL-R-DESPAC           PIC X(30).    ; Direcci¢n Despacho
   03 CL-R-CONDIC           PIC 9(03).    ; C¢digo Condici¢n de Venta
   03 CL-R-DCT-BA           PIC 9(04).    ; Porcentaje Descuento Base
   03 CL-R-VENDED           PIC 9(05).    ; C¢digo Vendedor
   03 CL-R-VISITA           PIC X(01).    ; D¡a de Visita
   03 CL-R-CIUDAD           PIC X(13).    ; Ciudad
   03 CL-R-RET              PIC 9 BINARY. ; Caracter Ret de carro  X'0D'
   03 CL-R-FLI              PIC 9 BINARY. ; Caracter Fin de linea  X'0A'
;  ----------------------------------
;  TOTAL                       98

FD PRODUCTO-F                 ; Archivo PRODUCTO.D = Productos
   RECORDING MODE IS FIXED
   TYPE IS 'D'.

01 PRODUCTO-R.
   03 PRD-CODIGO            PIC 9(06).    ; C¢digo Num‚rico
   03 PRD-DESCRI            PIC X(30).    ; Descripci¢n de Producto
   03 PRD-PR-VTA            PIC 9(05).    ; Precio Unidad de Venta Actual
   03 PRD-UN-BAS            PIC 9(04).    ; Unidades B sicas por Unidad de Venta
   03 PRD-DCT-PR            PIC 9(04).    ; Porcentaje Descuento Promocional
   03 PRD-OFERTA            PIC X(01).    ; Marca Producto en Oferta
   03 PRD-RET               PIC 9 BINARY. ; Caracter Ret de carro  X'0D'
   03 PRD-FLI               PIC 9 BINARY. ; Caracter Fin de linea  X'0A'
;  ----------------------------------
;  TOTAL                       51

FD COND-VTA-F                  ; Archivo COND-VTA.D = Condici¢n de Venta
   RECORDING MODE IS FIXED
   TYPE IS 'D'.

01 COND-VTA-R.
   03 CON-CODIGO            PIC 9(03).    ; C¢digo Condici¢n de Venta
   03 CON-DESCRI            PIC X(20).    ; Descripci¢n Condici¢n de Venta
   03 CON-PL-VEN            PIC 9(03).    ; Plazo D¡as de Vencimiento
   03 CON-RET               PIC 9 BINARY. ; Caracter Ret de carro  X'0D'
   03 CON-FLI               PIC 9 BINARY. ; Caracter Fin de linea  X'0A'
;  ----------------------------------
;  TOTAL                       27

FD EX-BODEG-F                  ; Archivo EX-BODEG.D = Existencia Bodega
   RECORDING MODE IS FIXED
   TYPE IS 'D'.

01 EX-BODEG-R.
   03 EXI-LL1.
      05 EXI-COD-BO         PIC 9(04).    ; C¢digo Bodega
      05 EXI-COD-PR         PIC 9(06).    ; C¢digo Producto

   03 EXI-INICIA            PIC 9(05).    ; Existencia Inicial
   03 EXI-SALDOO            PIC 9(05).    ; Saldo Existencia (Actual)
   03 EXI-UL-CAN            PIC 9(04).    ; Ultimo Canje
   03 EXI-CANJEA            PIC 9(04).    ; Total Canjeado Acumulado
   03 EXI-RET               PIC 9 BINARY. ; Caracter Ret de carro  X'0D'
   03 EXI-FLI               PIC 9 BINARY. ; Caracter Fin de linea  X'0A'
;  ----------------------------------
;  TOTAL                       29


FD SO-ABAST-F                  ; Archivo SO-ABAST.D = Solicitud Abastecimiento
   RECORDING MODE IS FIXED
   TYPE IS 'D'.

01 SO-ABAST-R.
   03 SOL-LL1.
      05 SOL-BODEGA         PIC 9(04).    ; C¢digo de Bodega
      05 SOL-PRODUC         PIC 9(06).    ; C¢digo Producto

   03 SOL-INICIA            PIC 9(05).    ; Existencia Inicial
   03 SOL-FINALL            PIC 9(05).    ; Saldo Existencia
   03 SOL-UN-BAS            PIC 9(05).    ; Cantidad Unidades B sicas
   03 SOL-TOTALL            PIC 9(04).    ; Total Canjeado
   03 SOL-RET               PIC 9 BINARY. ; Caracter Ret de carro  X'0D'
   03 SOL-FLI               PIC 9 BINARY. ; Caracter Fin de linea  X'0A'
;  ----------------------------------
;  TOTAL                       30

FD CABECERA-F                  ; Archivo CABECERA.D = Cabecera
   RECORDING MODE IS FIXED
   TYPE IS 'D'.

01 CABECERA-R.
   03 CAB-LL1.
      05 CAB-FACTURA        PIC 9(07).    ; N£mero de Factura

   03 CAB-LL2.
      05 CAB-LOC-CLI        PIC 9(02).    ; Local Cliente
      05 CAB-RUT-CLI.
         07 CAB-RUT-RUT     PIC 9(08).    ; Rut de Cliente
         07 CAB-RUT-DIG     PIC X(01).    ; D¡gito Verificador

   03 CAB-FEC-FAC           PIC 9(06).    ; Fecha Emisi¢n Factura (AAMMDD)
   03 CAB-HOR-FAC           PIC 9(04).    ; Hora Factura (HHMM)
   03 CAB-FEC-VEN           PIC 9(06).    ; Fecha Vencimiento Factura
   03 CAB-COD-VEN           PIC 9(05).    ; C¢digo de Vendedor
   03 CAB-CON-VEN           PIC 9(03).    ; C¢digo Condici¢n Venta
   03 CAB-VAL-IVA           PIC 9(07).    ; Valor IVA
   03 CAB-VAL-ILA           PIC 9(07).    ; Valor ILA
   03 CAB-TOT-FAC           PIC 9(07).    ; Total Factura (Neto + ILA + IVA)
   03 CAB-FAC-IMP           PIC X(01).    ; Factura Impresa
   03 CAB-FAC-NUL           PIC X(01).    ; Factura Nula
   03 CAB-TIP-CLI           PIC X(01).    ; Tipo Cliente
   03 CAB-RET               PIC 9 BINARY. ; Caracter Ret de carro  X'0D'
   03 CAB-FLI               PIC 9 BINARY. ; Caracter Fin de linea  X'0A'
;  ----------------------------------
;  TOTAL                       67


FD DETALLES-F                  ; Archivo DETALLES.D = Detalles
   RECORDING MODE IS FIXED
   TYPE IS 'D'.

01 DETALLES-R.
   03 DET-LL1.
      05 DET-FACTURA        PIC 9(07).    ; N£mero de Factura
      05 DET-PRODUCT        PIC 9(06).    ; C¢digo Producto

   03 DET-CAN-UNI           PIC 9(04).    ; Cantidad Unidades B sicas
   03 DET-VAL-BRU           PIC 9(07).    ; Valor Bruto Producto (CANTIDAD * PRECIO)
   03 DET-DES-PRO           PIC 9(04).    ; Porcentaje Descuento Promocional Producto
   03 DET-VAL-PRO           PIC 9(07).    ; Valor Descuento Promocional
   03 DET-DES-BAS           PIC 9(04).    ; Porcentaje Descuento Base Producto
   03 DET-VAL-BAS           PIC 9(07).    ; Valor Descuento Base Producto
   03 DET-RET               PIC 9 BINARY. ; Caracter Ret de carro  X'0D'
   03 DET-FLI               PIC 9 BINARY. ; Caracter Fin de linea  X'0A'
;  ----------------------------------
;  TOTAL                       47


;ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¸
;³                FIN DESCRIPCION ESTRUCTURA DE ARCHIVOS                 ³
;ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾


WORKING-STORAGE SECTION.
01 VARIABLES-DECLARATIVES.

   03  LECTURA-VALIDA    PIC 9(01).
   03  RESP-PANTALLA     PIC X(64).    ; Respaldo Pantalla Power-Off
   03  DISP-SAVE         PIC X(64).    ; Respaldo Pantalla Consulta con WAIT
   03  DISPLAY-SAVE      PIC X(64).    ; Respaldo Pantalla Consulta con ACTION
   03  WS-MEM-AV         PIC 9(06).    ; Memoria Disponible
   03  FLAG-HORA         PIC 9 BINARY. ; Habilitacion de Luz de pantalla
   03  IND-TECLA-FN      PIC 9 BINARY. ; Habilitacion de Consulta con ACTION
   03  PASSW-ZWS         PIC X(15).
   03  VAR-FECHA-HORA.
       04 FECHA-EMPAQUETADA PACKED.
          05 ano          pic 9(02) packed.
          05 mes          pic 9(02) packed.
          05 dia          pic 9(02) packed.
          05 dow          pic 9(02) packed. ; dia de la semana (1 a 7)
          05 hora         pic 9(02) packed.
          05 minu         pic 9(02) packed.
          05 seg          pic 9(02) packed.
          05 ampm         pic 9(02) packed. ; 0 = AM   1 = PM
          05 formato-1224 pic 9(02) packed. ; 0 => 0 a 12 Hrs
                                            ; 1 => 0 a 24 Hrs
       04 fecha-normal.
          05 ano-ws       pic 9(02).
          05 mes-ws       pic 9(02).
          05 dia-ws       pic 9(02).
          05 dow-ws       pic 9(02).
          05 hora-ws      pic 9(02).
          05 minu-ws      pic 9(02).
          05 seg-ws       pic 9(02).
          05 ampm-ws      pic 9(02).
          05 form-1224-ws pic 9(02).

01 VARIABLES-MANEJO-MENUES.
   03 INDICE            PIC 99 BINARY.
   03 INDICE-OPC        pic 99 binary.
   03 MIN-INDICE        PIC 99 BINARY.
   03 MAX-INDICE        PIC 99 BINARY.
   03 IND-PANTALLA      PIC 9  BINARY.
   03 IND-MENU1         PIC 99 BINARY.
   03 IND-MENU2         PIC 99 BINARY.
   03 IND-MENU3         PIC 99 BINARY.
   03 IND-MENU4         PIC 99 BINARY.
   03 IND-MENU5         PIC 99 BINARY.
   03 IND-MENU6         PIC 99 BINARY.
   03 IND-MENU7         PIC 99 BINARY.
   03 IND-MENU8         PIC 99 BINARY.
   03 IND-MENU9         PIC 99 BINARY.

01 VAR-MENSAJE1.
   03 MSG-TXT           PIC 9(40) DISPLAY.

01 VAR-MENSAJE2 REDEFINES.
   03 MSG-COMM.
     05 MENS-COM-1      PIC 9(06) DISPLAY. ;ERROR
     05 MENS-COM-2      PIC 9(02) DISPLAY. ;XX, COM-ERR
     05 MENS-COM-3      PIC 9(32) DISPLAY. ;Descripcion de error.

01 VARIABLES-PUNTERO-DATOS.
   03 PUNT-INICIO    PIC 9(6) BINARY.
   03 PUNT-FIN       PIC 9(6) BINARY.
   03 LARGO-REGISTRO PIC 9(6) BINARY.
   03 LARGO-ARCHIVO  PIC 9(6) BINARY.
   03 NRO-REG        PIC 9(3) BINARY.

01  CMD-REC-WS.     ;Registro de envio de comandos sign ON/OFF
    03 CMD.
       05 FILLER        PIC 9.
       05 CMDLO         PIC 9.          ;low byte of command

    03 status           PIC 99.         ;sign in status
    03 terminal         PIC 9.          ;terminal type
    03 password         PIC 9(7).       ;operator password
    03 filler           PIC 9(18).

01  cmd-file-name-ws    redefines. ; Registro de envio/recepcion comandos
    03 filler           pic 99.    ; transmision de archivos al Host
    03 filename         pic X(10).
    03 filler           pic x(18).

01  file-hdr-rcv        redefines. ; Registro de envio/recepcion comandos
    03 filler           pic x(8).  ; recepcion de archivos al Host
    03 rcv-file-type    pic x(2).
    03 rcv-file-name    pic x(16).
    03 filler           pic x(4).

01 Variables-comunica.
   03 GP-TIMER-WS         PIC 9(4).
   03 cmd-aux-ws       pic 99.
   03 largo-arch       pic 9(6) binary.

01 var-com-env.
   03 file-to-send.
      05 arch-a-enviar pic X(8).
      05 s-punto       pic X.
      05 s-extension   pic X.

01 var-com-rec redefines.
   03 file-to-receive.
      05 arch-a-recibir pic X(8).
      05 r-punto        pic X.
      05 r-extension    pic X.

;**********************************************************************
;*   VARIABLES SISTEMA
;**********************************************************************

01 VARIABLES-SISTEMA.
   03 WW-CON-VTA-CAB   PIC 9(03).
   03 MODO-OPERACION   PIC 9(01). ; 1= Ingresa, 2= Modifica
   03 AUX-FACTURA      PIC 9(07).
   03 WW-DCT-PRODU     PIC 9(04).
   03 WW-DCT-CLIEN     PIC 9(04).

   03 AUX-FILE-NAME.
      05 AUX-F-N-PREF  PIC X(03).
      05 AUX-F-N-VEND  PIC 9(05).

   03 GRABA-CLI-NUEV     PIC 9(01).
   03 W-CL-C-LL1.
      05 W-CL-C-LOCAL    PIC 9(02).    ; Local
      05 W-CL-C-RUT.                   ; RUT
         07 W-EL-RUT     PIC 9(08).
         07 W-EL-DIG     PIC X(01).

   03 WW-AUX-FACTUR      PIC 9(07).
   03 AUX-BLANCO         pic x(01).
   03 TXT-DATOS          pic X(16).
   03 WW-PASSWORD        PIC X(04).
   03 W-IDE-PASSWO       PIC X(04).
   03 W-IDE-BODEGA       PIC 9(04).
   03 W-IDE-NOMBRE       PIC X(17).
   03 W-IDE-VENDED       PIC 9(05).
   03 W-IDE-VISITA       PIC X(01).
   03 W-IDE-BASE         PIC 9(04).
   03 W-IDE-IVA          PIC 9(04).
   03 W-IDE-ILA          PIC 9(04).
   03 W-IDE-FACTUR       PIC 9(07).
   03 W-CL-C-LOC         PIC 9(02).
   03 W-CL-C-RAZON       PIC X(16).
   03 W-CL-C-CONDIC      PIC 9(03).
   03 W-CL-R-CONDIC      PIC 9(03).
   03 W-NEXT-RAZON       PIC X(30).

01 VAR-HJADFSJK.
   03 AUX-RES-NRO-FAC    PIC 9(07).
   03 AUX-LINEAS-MAX     PIC 9(02).
   03 W-NEXT-CONDI       PIC X(12).
   03 W-NEXT-VENCI       PIC 9(03).
   03 W-CON-VTA-COD      PIC 9(03).
   03 W-CON-VTA-DES      PIC X(16).
   03 W-PRD-CODIGO.
      05 W-PRD-UNO       PIC 9(04).
      05 W-PRD-DOS       PIC 9(02).


   03 W-PRD-DESCRI       PIC X(16).
   03 W-PRD-DESCRI-9     PIC X(09).
   03 W-PRD-PRECIO       PIC 9(05).
   03 W-PRD-CANTID       PIC 9(04).
   03 W-PRD-CAN-AN       PIC 9(04).
   03 W-TOT-PROD         PIC 9(07).
   03 W-TOT-FACT         PIC 9(07).
   03 W-TOT-IVA          PIC 9(12).
   03 W-TOT-ILA          PIC 9(12).
   03 W-TOTAL-IVA        PIC 9(07).
   03 W-TOTAL-ILA        PIC 9(07).
   03 W-TOT-CANTID       PIC 9(06).
   03 W-PRO-STOCK        PIC 9(05).

01 VARIABLES-DE-CLIENTES.
   03 W-PRECIO-UB        PIC 9(07).
   03 W-PRECIO-UV        PIC 9(07).
   03 W-PR-UN-VTA        PIC 9(07).
   03 W-PR-UN-BAS        PIC 9(07).

   03 W-CLI-LOCAL        PIC 9(02).
   03 W-CLI-RUT          PIC 9(08).
   03 W-CLI-DIG          PIC X(01).
   03 W-CLI-RAZON        PIC X(30).
   03 W-CLI-DESPAC       PIC X(30).
   03 W-CLI-CONDIC       PIC 9(03).
   03 W-CLI-DCT-BA       PIC 9(04).
   03 W-CLI-VENDED       PIC 9(05).
   03 W-CLI-VISITA       PIC X(01).

   03 W-DET-LL1.
      05 W-DET-FACTURA   PIC 9(07).
      05 W-DET-PRODUCT   PIC 9(06).

   03 W-CAB-LL1.
      05 W-CAB-FACTURA   PIC 9(07).

   03 AUX-LL1-EX-BODEGA.
      05 AUX-LL1-BOD     PIC 9(04).
      05 AUX-LL1-PRD     PIC 9(06).


01 VARIABLES-PARA-CALCULO.
   03 CALC-VAL-BRU-PRD   PIC 9(07).
   03 CALC-VAL-DCT-PRM   PIC 9(07).
   03 CALC-VAL-DCT-BAS   PIC 9(07).
   03 CALC-VAL-DCT-AUX   PIC 9(07).

01 VARIABLES-CLI-NUEVO.
   03 W-CL-N-LL1.
      05 W-CL-N-LOCAL    PIC 9(02).    ; Local
      05 W-CL-N-RUTTT.
         07 W-CL-N-RUT   PIC 9(08).    ; RUT
         07 W-CL-N-DIG   PIC X(01).    ; Digito

   03 W-CL-N-RAZON       PIC X(30).    ; Raz¢n Social
   03 W-CL-N-DESPAC      PIC X(30).    ; Direcci¢n Despacho
   03 W-CL-N-CONDIC      PIC 9(03).    ; C¢digo Condici¢n de Venta
   03 W-CL-N-DCT-BA      PIC 9(04).    ; Porcentaje Descuento Base
   03 W-CL-N-VENDED      PIC 9(05).    ; C¢digo Vendedor
   03 W-CL-N-VISITA      PIC X(01).    ; D¡a de Visita
   03 W-CL-N-CIUDAD      PIC X(13).

01 VARIABLES-DE-FECHA.

   03 WW-DIA            PIC 9(02).
   03 WW-MES            PIC 9(02).
   03 WW-ANO            PIC 9(02).

   03 WW-HOR            PIC 9(02).
   03 WW-MIN            PIC 9(02).

   03 AUX-FECHA-FILE.
      05 AUX-WW-ANO     PIC 9(02).
      05 AUX-WW-MES     PIC 9(02).
      05 AUX-WW-DIA     PIC 9(02).

   03 AUX-HORA-FILE.
      05 AUX-WW-HH      PIC 9(02).
      05 AUX-WW-MM      PIC 9(02).


01 VARIABLES-CANJE-ABAST.
   03 CANJ-CAN-CANJE    PIC 9(05).
   03 CANJ-ULT-CANJE    PIC 9(05).
   03 CANJ-ACU-CANJE    PIC 9(05).
   03 CANJ-COD-PRD      PIC 9(06).
   03 CANJ-DES-PRD      PIC X(30).

   03 ABAS-COD-PRD      PIC 9(06).
   03 ABAS-DES-PRD      PIC X(30).

   03 W-EXI-SALDOO      pic 9(05).
   03 W-EXI-INICIA      pic 9(04).
   03 W-EXI-CANJEA      pic 9(04).


01 VARIABLES-IMPRESION.
   03 IMP-CAB-FAC       PIC 9(07).
   03 IMP-CAB-LOC       PIC 9(02).
   03 IMP-CAB-RUT       PIC 9(08).
   03 IMP-CAB-DIG       PIC X(01).
   03 IMP-CAB-NOM       PIC X(16).
   03 IMP-CAB-RAZ       PIC X(30).
   03 IMP-CAB-DIR       PIC X(30).
   03 IMP-CAB-DES       PIC X(30).
   03 IMP-CAB-CIU       PIC X(13).
   03 IMP-CAB-TOT       PIC 9(07).
   03 IMP-CAB-FEC       PIC 9(06).
   03 IMP-CAB-HOR       PIC 9(04).

   03 PRN-CAB-FAC       PIC 9(07).
   03 PRN-CAB-NUM       PIC 9(07).
   03 PRN-CAB-FEC.
      05 PRN-FEC-FAC-AA    PIC 9(02).
      05 PRN-FEC-FAC-MM    PIC 9(02).
      05 PRN-FEC-FAC-DD    PIC 9(02).

   03 PRN-CAB-VEN.
      05 PRN-FEC-VEN-AA    PIC 9(02).
      05 PRN-FEC-VEN-MM    PIC 9(02).
      05 PRN-FEC-VEN-DD    PIC 9(02).

   03 PRN-CAB-HOR       PIC 9(04).
   03 PRN-CAB-RUT       PIC X(09).
   03 PRN-CAB-RAZ       PIC X(30).
   03 PRN-CAB-LOC       PIC 9(02).
   03 PRN-CAB-DIR       PIC X(30).

01 VAR-HJFHJFKD.
   03 PRN-CAB-DES       PIC X(30).
   03 PRN-CAB-CIU       PIC X(13).
   03 PRN-CAB-VED       PIC 9(05).
   03 PRN-CAB-IVA       PIC 9(12).
   03 PRN-CAB-ILA       PIC 9(12).

01 VAR-JHJKDSJK.
   03 PRN-DET-COND      PIC X(20).
   03 PRN-DET-PRD       PIC 9(06).
   03 PRN-DET-DES       PIC X(30).
   03 PRN-DET-CAN       PIC 9(04).
   03 PRN-DET-UNI       PIC 9(07).
   03 PRN-DET-NET       PIC 9(07).
   03 PRN-DET-BRU       PIC 9(07).
   03 PRN-DET-DES-PRO   PIC 9(04).
   03 PRN-DET-VAL-PRO   PIC 9(07).
   03 PRN-DET-DES-BAS   PIC 9(04).
   03 PRN-DET-VAL-BAS   PIC 9(07).

   03 PRN-TOTAL-NETO    PIC 9(12).
   03 PRN-TOTAL-ILA     PIC 9(12).
   03 PRN-TOTAL-IVA     PIC 9(12).

01 VARIABLES-TOTALES.
   03 TOT-DET-BRUTO     PIC 9(07).

01 VAR-CALCULO-II.
   03 TOTAL-A           PIC 9(12).
   03 TOTAL-B           PIC 9(12).
   03 TOTAL-C           PIC 9(12).
   03 TOTAL-D           PIC 9(12).
   03 TOTAL-E           PIC 9(12).
   03 TOTAL-F           PIC 9(12).
   03 TOTAL-G           PIC 9(12).

01 RUT-1                redefines.
   03 WS-NUM            PIC 9(01) occurs 8 times.

01 OTROS.
   03 CLI-RUT-WS.
      05 cli-rut1-ws    pic 9(08).
      05 CLI-DIG-WS     pic x(01).

   03 rest-aux.
      05 resto-aux1     pic 9(02).
      05 resto-aux      pic 9(02) binary.

   03 ws-resto.
      05 ws-resto1      pic 9(03) binary.

   03 ws-dig            pic x(01).
   03 WS-CON            pic 9(02) binary.
   03 ws-tbl            pic 9(02) binary.
   03 ws-suma           pic 9(03) binary.
   03 WS-NUM-AUX        pic 9(03) binary.



01 VAR-SHOW-IDENTIF.
   03 WW-IDE-VENDED      PIC 9(05).    ; C¢digo de Vendedor
   03 WW-IDE-NOMBRE      PIC X(30).    ; Nombre de Vendedor
   03 WW-IDE-BODEGA      PIC 9(04).    ; C¢digo de Bodega
   03 WW-IDE-PJ-IVA      PIC 9(04).    ; Porcentaje IVA
   03 WW-IDE-PJ-ILA      PIC 9(04).    ; Porcentaje ILA
   03 WW-IDE-POR-IN      PIC 9(04).    ; Porcentaje Descuento Base Inicial
   03 WW-IDE-CON-IN      PIC 9(03).    ; C¢digo Condici¢n de Venta Inicial
   03 WW-IDE-VISITA      PIC X(01).    ; D¡a de Visita
   03 WW-IDE-FAC-IN      PIC 9(07).    ; N£mero de Factura Inicial
   03 WW-IDE-FAC-FI      PIC 9(07).    ; N£mero de Factura Final
   03 WW-IDE-FAC-EM      PIC 9(07).    ; N£mero Ultima Factura Emitida


01 TABLA-DIA REDEFINES.
   03 ARR-DIA            PIC 9(02) OCCURS 12 TIMES.
   03 NUM-MES            PIC 9(02) BINARY.
   03 X                  PIC 9(02) BINARY.
   03 NUM-LIN-DETALLE    PIC 9(02).
   03 NUM-LIN-27         PIC 9(02).
   03 XXX                PIC 9(02).

01 VAR-RE-CALCULO-CABECERA.
   03 RE-WW-TOTAL-IVA    PIC 9(12).
   03 RE-WW-TOTAL-ILA    PIC 9(12).
   03 RE-WW-TOTAL-FAC    PIC 9(12).


CHECK section.

CH  CK-DIA
    type is range
    size is 2
    occurs 1 times
    value is '01' '31' OK.

CH  CK-MES
    type is range
    size is  2
    occurs   1 times
    value is '01' '12' OK.

CH  CK-YEAR
    type is range
    size is 2
    occurs 1 times
    value is '86' '99' OK.

CH  CK-HRS
    type is range
    size is 2
    occurs 1 times
    value is '00' '24' OK.

CH  CK-MIN
    type is range
    size is 2
    occurs 1 times
    value is '00' '60' OK.

;**********************************************************************
;*                       DEFINICION DE TECLADO                        *
;**********************************************************************

KEYBOARD section.

kd      KBD-45        value is

;unshifted

   OFF     UAR    KSTOCK DAR    KEND
   KDEL    ER      ER    DEL    KNEXT
    ER     ER     KMOD  LIGHT  KMEM
    ER     ER      ADJ   ER    KRELOJ
    ER     ER      ER    ER     ER
 SHIFT     ER      ER    ER     CLR
   '7'     '8'     '9'   BAR
   '4'     '5'     '6'   FAR
   '1'     '2'     '3'   ENTER
    NO     '0'     '.'

;shifted

   OFF    'A'   'B'     'C'   KEND
   'D'    'E'   'F'     'G'  'H'
   'I'    'J'   'K'     'L'  'M'
   'N'    'O'   'P'     'Q'  'R'
   'S'    'T'   'U'     'V'  'W'
SHIFT     'X'   'Y'     'Z'  CLR
  '7'     '8'   '9'    BAR
  '4'     '5'   '6'    FAR
  '1'     '2'   '3'  ENTER
  '-'     '0'   ' '.

KD       KBD-35   value is

;unshifted

        OFF     UAR     DAR     KEND
        KSTOCK  ER      ER      KMOD
        KDEL    ER      ER      KNEXT
        ER      ADJ     ER      KMEM
        shift   ER      ER      CLR
        '7'     '8'     '9'     BAR
        '4'     '5'     '6'     FAR
        '1'     '2'     '3'     ENTER
        NO      '0'     '.'

;shifted

        off     'A'     'B'     KEND
        'C'     'D'     'E'     'F'
        'G'     'H'     'I'     'J'
        'K'     'L'     'M'     'N'
        shift   'O'     'P'     CLR
        'Q'     'R'     'S'     BAR
        'T'     'U'     'V'     FAR
        'W'     'X'     'Y'     ENTER
        ' '     'Z'     '.'.

;**********************************************************************
;*                       DEFINICION DE PANTALLAS                      *
;**********************************************************************
SCREEN SECTION.

; PANTALLAS ASOCIADAS A DECLARATIVES
;--------------------------------------
sd  MEM-REM-S.                                  ; Muestra disponibilidad de
    03 WS-MEM-AV                                        ; memoria.
       prompt 17, '         Bytes  '
       mask   19, 'ZZZZZ9'
       edit.
;--------------------------------------
;-   PANTALLAS ASOCIADAS A LA FECHA   -
;--------------------------------------
SD Fecha-SD.
   03 Dia-ws
      edit
      prompt 2, 2, 'Fecha:'
      mask 9, 2, '99'
      check is ck-dia
      numeric
      min 1
      auto-record.
   03 Mes-ws
      edit
      prompt 11, 2, '/'
      mask 12, 2, '99'
      check is ck-mes
      numeric
      min 1
      auto-record.
   03 Ano-ws
      edit
      prompt 14, 2, '/'
      mask 15, 2, '99'
      numeric
      min 1
      auto-record.

SD Fecha2-SD.
   03 Mes-ws
      edit
      prompt 11, 2, '/'
      mask 12, 2, '99'
      check is ck-mes
      numeric
      min 1
      auto-record.
   03 Ano-ws
      edit
      prompt 14, 2, '/'
      mask 15, 2, '99'
      numeric
      auto-record.

SD Fecha3-SD.
   03 Ano-ws
      edit
      prompt 14, 2, '/'
      mask 15, 2, '99'
      numeric
      auto-record.

SD Hora-SD.
   03 Hora-ws
      edit
      prompt 2, 3, 'Hora :'
      mask 9, 3, '99'
      check is CK-HRS
      numeric
      auto-record.
   03 Minu-ws
      edit
      prompt 11, 3, ':'
      mask 12, 3, '99'
      check is ck-min
      numeric
      auto-record.
   03 Seg-ws
      edit
      prompt 14, 3, ':'
      mask 15, 3, '99'
      check is ck-min
      numeric
      auto-record.

SD Hora2-SD.
   03 Minu-ws
      edit
      prompt 11, 3, ':'
      mask 12, 3, '99'
      check is ck-min
      numeric
      auto-record.
   03 Seg-ws
      edit
      prompt 14, 3, ':'
      mask 15, 3, '99'
      check is ck-min
      numeric
      auto-record.

SD Hora3-SD.
   03 Seg-ws
      edit
      prompt 14, 3, ':'
      mask 15, 3, '99'
      check is ck-min
      numeric
      auto-record.
;*****************************************************************************
; Pantallas asociadas a rutinas de eliminacion de archivos del directorio
;*****************************************************************************

sd MEM-AVAIL-SD.
   03 WS-MEM-AV
      edit
      prompt 1, 4, 'Disp.       byte'
      mask   6, 4, 'ZZZZZZ'.

sd File-Name-SD.
   03 File-Name
      mask 1, 2, 'XXXXXXXX'.
   03 File-Type
      prompt 9, 2, '.'
      mask   10, 2, 'X'.
   03 Largo-Archivo
      edit
      prompt 8, 3, 'Bytes'
      mask  1, 3, 'ZZZZZZ'.

;****************************************
;* PANTALLAS ASOCIADAS A COMUNICACIONES *
;****************************************

SD ARCH-A-ENVIAR-SD.
03 FILE-TO-SEND
   clear
   edit
   prompt 17, '  Enviando -->  '
   mask 36, 'XXXXXXXXXX'.

03 LARGO-ARCH
   edit
   prompt 58, 'bytes'
   mask   51, 'ZZZZZ9'.

SD ARCH-A-RECIBIR-SD.
03 FILE-TO-RECEIVE
   clear
   edit
   prompt 17, ' Recibiendo<--  '
   mask 36, 'XXXXXXXXXX'.

SD LARGO-ARCH-SD.
03 LARGO-ARCH
   edit
   prompt 58, 'bytes'
   mask   51, 'ZZZZZ9'.

SD LINEA-MSG.
03 MSG-TXT
   clear 4
   mask 1, 4, 'XXXXXXXXXXXXXXXX' IN INVERSE VIDEO.

SD GP-TIMER-SD.
03 GP-TIMER-WS
   PROMPT 3, 3, 'TIMER:'
   MASK  10, 3, '9999'.

;PANTALLAS TEMPORALES.

;*************************************************************
;* PANTALLAS DEL SISTEMA
;*************************************************************

sd LOOP-CLI-CARTERA.
03 W-EL-RUT
   edit
   prompt  9  2 '-'
   mask    1  2 'ZZZZZZZ9'.
03 W-EL-DIG
   edit
   prompt 11  2 '    '
   mask   10  2 'X'.
03 W-CL-C-LOCAL
   edit
   mask   15  2  '99'.
03 W-CL-C-RAZON
   edit
   mask    1  3 'XXXXXXXXXXXXXXXX'.
03 W-CL-C-CONDIC
   edit
   prompt  4  4 ' '
   mask    1  4 'ZZ9'.
03 W-NEXT-CONDI
   edit
   mask    5  4 'XXXXXXXXXXXX'.

SD WW-PASSWORD-SD.
03 WW-PASSWORD
   clear
   auto-shift
   edit
   prompt  1  1 '    Password       Supervisor         ****      '
   mask    7  4 'BBBB'.

sd W-IDE-FACTUR-sd.
03 W-IDE-FACTUR
   edit
   prompt  1  1 'Num:            '
   mask   10  1 'ZZZZZZ9'.

sd W-NUE-NRO-FACT-sd.
03 W-IDE-FACTUR
   edit
   prompt  1  2 'Nuevo:          '
   mask   10  2 'ZZZZZZ9'.

sd ING-CLI-RUT-SD.
03 W-EL-RUT
   edit
   prompt  1  2 'Ruc:          - '
   mask    7  2 'ZZZZZZZ9'.

sd ING-CLI-DIG-SD.
03 W-EL-DIG
   edit
   mask   16  2 'X'.

sd ING-CLI-COD-SD.
03 W-CL-C-LOCAL
   edit
   prompt  1  3 'Sucursal:       '
   mask   15  3 'Z9'.

sd W-NEXT-RAZON-sd.
03 W-NEXT-RAZON
   clear
   edit
   prompt  1  1 '  Razon Social  '
   mask    1  3 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

sd W-NEXT-CONDI-sd.
03 W-NEXT-CONDI
   auto-shift
   clear
   edit
   prompt  1  1 'Condicion  Venta'
   mask    1  3 'XXXXXXXXXXXXXXXXXXXX'.
03 W-NEXT-VENCI
   edit
   prompt  1  4 'Dia Vencim.     '
   mask   14  4 'ZZ9'.

sd W-CON-VTA-COD-sd.
03 W-CON-VTA-COD
   edit
   prompt  1  3 'Cond. Vta. :    '
   mask   14  3 'ZZ9'.


sd W-CON-VTA-DES-sd.
03 W-CON-VTA-DES
   edit
   mask    1  4 'XXXXXXXXXXXXXXXX'.

sd W-PRD-CODIGO-sd.
03 W-PRD-CODIGO
   clear
   edit
   prompt  1  1 'Prod:           '
   mask   11  1 'ZZZZZ9'.

sd W-PRD-CODIG2-sd.
03 W-PRD-CODIGO
   edit
   prompt  1  1 'Prod:           '
   mask   11  1 'ZZZZZ9'.

sd W-PRD-DESCRI-sd.
03 W-PRD-DESCRI
   edit
   mask    1  2 'XXXXXXXXXXXXXXXX'.

sd W-PRO-STOCK-sd.
03 W-PRO-STOCK
   edit
   prompt  1  3 'Stock:          '
   mask   12  3 'ZZZZ9'.

sd W-COD-DES-PROD-sd.
03 W-PRD-CODIGO
   edit
   mask    1  1 'ZZZZZ9'.
03 W-PRD-DESCRI-9
   edit
   mask    8  1 'XXXXXXXXX'.

sd W-PRECIO-UB-sd.
03 W-PRECIO-UB
   edit
   prompt  1  2 'Prec. UB:'
   mask   10  2 'ZZZZZZ9'.

sd W-PRECIO-UV-sd.
03 W-PRECIO-UV
   edit
   prompt  1  3 'Prec. UV:'
   mask   10  3 'ZZZZZZ9'.

sd TOTAL-DE-PRODUCTO-sd.
03 W-PRD-CANTID
   edit
   prompt  1  2 'Cantidad:       '
   mask   13  2 'ZZZ9'.
03 W-TOT-PROD
   edit
   prompt  1  3 'Tot.Prod:       '
   mask   10  3 'ZZZZZZ9'.
03 W-TOT-FACT
   edit
   prompt  1  4 'Tot.Fact:       '
   mask   10  4 'ZZZZZZ9'.

sd W-PRD-PRECIO-sd.
03 W-PRD-PRECIO
   edit
   prompt  1  3 'P.Venta $       '
   mask   11  3 'ZZ.ZZ9'.

sd SHOW-PRECIO-SD.
03 W-PRD-PRECIO
   edit
   prompt  1  2 'P.Venta $       '
   mask   11  2 'ZZ.ZZ9'.

sd W-PRD-CANTID-sd.
03 W-PRD-CANTID
   edit
   prompt  1  4 'Cantidad        '
   mask   13  4 'ZZZ9'.

sd W-TOT-CANTID-sd.
03 W-TOT-CANTID
   edit
   prompt  1  4 'T.Cant.  ZZZ.ZZZ'
   mask   10  4 'ZZZ.ZZ9'.

sd TXT-DATOS-sd.
03 TXT-DATOS
   edit
   mask  1  1 'XXXXXXXXXXXXXXXX'.

sd STOCK-DEL-PROD-sd.
03 W-PRO-STOCK
   edit
   clear
   prompt  1  1 ' Stock Producto '
   mask    6  3 'ZZZZ9'.

sd W-CL-N-RUT-sd.
03 W-CL-N-RUT
   edit
   prompt  1  1 'RUC           -'
   mask    7  1 'ZZZZZZZ9'.

sd W-CL-N-DIG-sd.
03 W-CL-N-DIG
   edit
   mask   16 1 'X'.

sd W-CL-N-LOCAL-sd.
03 W-CL-N-LOCAL
   edit
   prompt  1  2 'Sucursal        '
   mask   15  2 'Z9'.

sd W-CL-N-RAZON-sd.
03 W-CL-N-RAZON
   auto-shift
   clear
   prompt  1  1 '  Razon Social  '
   mask    1  3 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

sd W-CL-N-DESPAC-sd.
03 W-CL-N-DESPAC
   auto-shift
   clear
   prompt  1  1 'Direcc. Despacho'
   mask    1  3 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

sd W-CL-N-CONDIC-sd.
03 W-CL-N-CONDIC
   edit
   prompt  1  1 'Cond. VTA.      '
   mask   14  1 'ZZ9'.

sd W-CL-N-DCT-BA-sd.
03 W-CL-N-DCT-BA
   edit
   prompt  1  2 'Dcto. Base %    '
   mask   13  2 'ZZZ9'.

sd W-CL-N-VENDED-sd.
03 W-CL-N-VENDED
   edit
   prompt  1  3 'Vendedor        '
   mask   12  3 'ZZZZ9'.

sd W-CL-N-VISITA-sd.
03 W-CL-N-VISITA
   edit
   prompt  1  4 'Visita          '
   mask   16  4 'X'.

sd W-CL-N-CIUDAD-sd.
03 W-CL-N-CIUDAD
   clear
   auto-shift
   prompt  1  1 '     Ciudad     '
   mask    2  3 'XXXXXXXXXXXXX'.

sd WW-DIA-sd.
03 WW-DIA
   edit
   prompt  1 1 '      /  /      '
   mask    5 1 '99'
   check is CK-DIA.

sd WW-MES-sd.
03 WW-MES
   edit
   mask    8 1 '99'
   check is ck-mes.

sd WW-ANO-sd.
03 WW-ANO
   edit
   mask   11 1 '99'.

sd WW-HOR-sd.
03 WW-HOR
   edit
   prompt  1 2 '       :        '
   mask    6 2 '99'.

sd WW-MIN-sd.
03 WW-MIN
   edit
   mask    9 2 '99'.

sd CANJ-COD-PRD-sd.
03 CANJ-COD-PRD
   edit
   prompt  1  1 'Codigo  :       '
   mask   11  1 'ZZZZZ9'.

sd CANJ-DES-PRD-sd.
03 CANJ-DES-PRD
   edit
   mask   1   2 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

sd CANJ-CAN-CANJE-sd.
03 CANJ-CAN-CANJE
   edit
   prompt  1  4 'Cantidad:       '
   mask   12  4 'ZZZZ9'.

sd CANJ-ACU-CANJE-sd.
03 CANJ-ACU-CANJE
   edit
   prompt  1  4 'Canje Acum:     '
   mask   12  4 'ZZZZ9'.

sd CANJ-ACU-2-CANJ-sd.
03 CANJ-ACU-CANJE
   edit
   prompt  1  3 'Canje Acum:     '
   mask   12  3 'ZZZZ9'.


sd IMP-CAB-FACTURA-sd.
03 IMP-CAB-FAC
   edit
   prompt  1  1 'Num:            '
   mask   10  1 'ZZZZZZ9'.
03 IMP-CAB-RUT
   edit
   prompt  9  2 '-'
   mask    1  2 'ZZZZZZZ9'.
03 IMP-CAB-DIG
   edit
   mask   10  2 'X'.
03 IMP-CAB-LOC
   edit
   prompt 11  2 '    '
   mask   15  2 'Z9'.
03 IMP-CAB-NOM
   edit
   mask    1  3 'XXXXXXXXXXXXXXXX'.
03 AUX-WW-DIA
   edit
   prompt  1  4 '  /  /  '
   mask    1  4 'Z9'.
03 AUX-WW-MES
   edit
   mask    4  4 '99'.
03 AUX-WW-ANO
   edit
   mask    7  4 '99'.
03 IMP-CAB-TOT
   edit
   prompt  9  4 ' '
   mask   10  4 'ZZZZZZ9'.

sd TOT-DETALE-SD.
03 TOT-DET-BRUTO
   clear
   edit
   prompt  1  1 '  Valor Bruto                    $  '
   mask    5  3 'Z.ZZZ.ZZ9'.


sd PNT-IDENT-1-sd.
03 WW-IDE-BODEGA
   edit
   prompt  1  1 'Cod.Bode:       '
   mask   13  1 'ZZZ9'.
03 WW-IDE-VENDED
   edit
   prompt  1  2 'Cod.Vend:       '
   mask   12  2 'ZZZZ9'.
03 WW-IDE-NOMBRE
   edit
   prompt  1  4 '                '
   mask    1  3 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

sd PNT-IDENT-2-sd.
03 WW-IDE-PJ-IVA
   edit
   prompt  1  1 'Val IGV        %'
   mask   11  1 'ZZ.Z9'.
03 WW-IDE-PJ-ILA
   edit
   prompt  1  2 'Val ILA        %'
   mask   11  2 'ZZ.Z9'.
03 WW-IDE-POR-IN
   edit
   prompt  1  3 'DCTO           %'
   mask   11  3 'ZZ.Z9'.
03 WW-IDE-CON-IN
   edit
   prompt  1  4 'Cond Inicial    '
   mask   14  4 'ZZ9'.

sd PNT-IDENT-3-sd.
03 WW-IDE-VISITA
   edit
   prompt  1  1 'Dia Visita      '
   mask   16  1 'X'.
03 WW-IDE-FAC-IN
   edit
   prompt  1  2 'F.Inici         '
   mask   10  2 'ZZZZZZ9'.
03 WW-IDE-FAC-FI
   edit
   prompt  1  3 'F.Final         '
   mask   10  3 'ZZZZZZ9'.
03 WW-IDE-FAC-EM
   edit
   prompt  1  4 'F.Emiti         '
   mask   10  4 'ZZZZZZ9'.


sd CON-PL-VEN-w-sd.
03 CON-PL-VEN
   clear
   edit
   mask  1 1 'ZZZ'.

sd FECHA-WWW-sd.
03 WW-DIA
   edit
   prompt 1 1 '  /  /  '
   mask   1 1 '99'.
03 WW-MES
   edit
   mask   4 1 '99'.
03 WW-ANO
   edit
   mask   7 1 '99'.

sd W-TOT-IVA-SD.
03 W-TOT-IVA
   clear
   edit
   mask 1 1 'Z.ZZZ.ZZ9'.

SD CALC-VAL-BRU-SD.
03 CALC-VAL-BRU-PRD
   clear
   EDIT
   PROMPT 1 1 'Bruto'
   MASK   7 1 'Z.ZZZ.ZZ9'.

sd CALC-VAL-PRM-SD.
03 CALC-VAL-DCT-PRM
   edit
   prompt 1 2 'DcPrm'
   MASK   7 2 'Z.ZZZ.ZZ9'.

sd CALC-VAL-BAS-SD.
03 CALC-VAL-DCT-BAS
   edit
   prompt 1 3 'DcBas'
   mask   7 3 'Z.ZZZ.ZZ9'.

sd DCTOS-sd.
03 WW-DCT-PRODU
   clear
   edit
   prompt 1 1 'Dcto PRD:'
   mask  11 1 '9999'.
03 WW-DCT-CLIEN
   edit
   prompt 1 2 'Dcto CLI:'
   mask  11 2 '9999'.

SD DETALLE-SD.
03 DET-PRODUCT
   edit
   prompt  1  1 'Prod:           '
   mask   11  1 'ZZZZZ9'.
03 W-PRD-DESCRI
   edit
   mask    1 2  'XXXXXXXXXXXXXXXX'.
03 DET-CAN-UNI
   edit
   prompt  1 3 'Cantidad        '
   mask   13 3 'ZZZ9'.

SD RE-WW-TOTAL-FAC-sd.
03 RE-WW-TOTAL-FAC
   EDIT
   PROMPT 1 1 'FAC:'
   MASK   5 1 'ZZZZZZ9'.

SD RE-WW-TOTAL-ILA-sd.
03 RE-WW-TOTAL-ILA
   EDIT
   PROMPT 1 3 'ILA:'
   MASK   5 3 'ZZZZZZ9'.

;ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»;
;º           S E C C I O N E S   D E  C O M U N I C A C I O N E S            º;
;ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼;

COMMUNICATION SECTION.

CD HOST-COM-96                        ; comunicacion directa
   type           is PTAP
   baud-rate      is 9600
   first-time     is 5 second
   nth-time       is 1000 ms
   block-delay    is 1
 ;  parity         is ODD
;   data-bits         7
;   stop-bits         1
;   mode           is ascii
   block contains 256 bytes.

CD snap-9600-com                      ; comunicacion directa
   type           is special
   baud-rate      is 9600
   first-time     is 5 second
   nth-time       is 120 ms
   block-delay    is 1
 ;  parity         is ODD
;   data-bits         7
;   stop-bits         1
;   mode           is ascii
   block contains 256 bytes.

CD IMPRESORA-ZFP-80                    ; Impresora ZFP-80
   type       is BURST
   baud-rate  is 9600
   parity     is NONE
   data-bits  8
   mode       is ASCII
   block contains 40 bytes.

REPORT section.

; nlit X'0C'      Avanza P gina
; nlit X'0D'      Salta l¡nea
; nlit X'0A'      Fin de L¡nea
; nlit X'0D0A'    Salta l¡nea y Fin de l¡nea
; lit  'Mensaje'  Escribe Texto

RD CABECERA-RP
   nlit X'1B0F'
   nlit X'0D'
   nlit X'0D'
   nlit X'0D'
   lit '                                '
   edit PRN-CAB-RAZ 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
   nlit X'0D'
   lit '                                '
   lit 'RUC: '
   edit PRN-CAB-RUT 'XXXXXXXX-X'
   lit '    SUC: '
   edit PRN-CAB-LOC 'Z9'
   nlit X'0D'
   lit '                                '
   edit PRN-CAB-DES 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
   nlit X'0D'
   lit '                                '
   edit PRN-CAB-CIU 'XXXXXXXXXXXXX'
   nlit X'0D'
   lit '                                           '
   lit '                                          '
   edit PRN-FEC-FAC-DD '99'
   lit '/'
   edit PRN-FEC-FAC-MM '99'
   lit '/'
   edit PRN-FEC-FAC-AA '99'
   lit '    '
   edit PRN-FEC-VEN-DD '99'
   lit '/'
   edit PRN-FEC-VEN-MM '99'
   lit '/'
   edit PRN-FEC-VEN-AA '99'
   nlit X'0D'
   nlit X'0D'
   nlit X'0D'
   nlit X'0D'
   lit '              '
   edit AUX-HORA-FILE '99:99'
   lit ' Hrs.        '
   edit PRN-CAB-DES 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
   lit '                                   '
   edit PRN-CAB-NUM 'ZZZZZZ9'
   lit '    '
   edit W-IDE-BODEGA 'ZZZ9'
   lit '  '
   edit W-IDE-VENDED 'ZZZZ9'
   lit ' '
   edit W-IDE-NOMBRE 'XXXXXXXXXXXXXXXXX'
   nlit X'0D'
   nlit X'0D'
   nlit X'0D'
   nlit X'0D'.

RD DETALLE-RP
   lit '     '
   edit PRN-DET-PRD 'ZZZZZ9'
   lit '  '
   edit PRN-DET-DES 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
   lit '           '
   edit PRN-DET-CAN 'ZZZ9'
   lit '  '
   edit PRN-DET-UNI 'Z.ZZZ.ZZ9'
   lit '                                                   '
   edit PRN-DET-NET 'Z.ZZZ.ZZ9'
   nlit X'0D'.

RD HORA-RP
   lit '     '
   edit PRN-DET-COND 'XXXXXXXXXXXXXXXXXXXX'
   lit '                      '
   lit '                                               '
   edit AUX-HORA-FILE '99:99'
   lit ' Hrs.'
   nlit X'0D'
   nlit X'0D'
   nlit X'0D'
   lit '                                               '
   lit '                                                          18%'
   nlit X'0D'
   nlit X'0D'.

RD TOTAL-RP
   lit '                                               '
   edit PRN-TOTAL-NETO 'Z.ZZZ.ZZ9'
   lit '          '
   edit PRN-CAB-ILA 'Z.ZZZ.ZZ9'
   lit '                             '
   edit PRN-CAB-IVA 'Z.ZZZ.ZZ9'
   lit '            '
   edit PRN-CAB-FAC 'Z.ZZZ.ZZ9'.

RD ESPACIOS-RP
   nlit X'0D'.

RD SALTO-PAG-RP
   nlit X'0C'.


;******************
PROCEDURE DIVISION.
;******************

declaratives.
DEC-1.
    when OFF    power-off
                perform POWER-MESSAGE.
    when TMOUT  alarm power-off         ; Si PTC inactivo --> OFF
                perform POWER-MESSAGE.
    when ADJ    Adjust-LCD DOWN.
    when KMEM   perform CHECK-MEMORY.   ; Muestra Memoria Disponible
    when LIGHT  perform DEC-LUZ.        ; Prende o apaga la luz
    when KRELOJ perform SHOW-RELOJ.
end declaratives.

INITIAL section.
;---------------

INIT-SYSTEM.
;-----------
     if KEYBOARD-SIZE = X'23'
        SET-KEYBOARD KBD-35
     else
        SET-KEYBOARD KBD-45
     end-if

     move 900   to USER-TMOUT         ; Inicializo TMOUT
     KEY-CLICK  is 1
     move 10    to FAST-SPEED         ; Inicializa Velocidad
     move 20    to SCROLL-SPEED       ; Inicializo Scrolling
     move 3000  to MEM-PROTECT        ; Inicializo Proteccion MEMORY
     SET-DECLARATIVES dec-1           ; Inicializo Declarativas
     move 0 to IND-TECLA-FN
     move 0 to FLAG-HORA
     move 1 to INDICE
     move 1 to IND-PANTALLA
     perform PRESENTACION
RETURN.

PRESENTACION.
;------------
    message 1 1 '   AUTOVENTA    '
    message 1 2 '  DEMOSTRACION  '
    message 1 3 '                '
    message 1 4 '    Ver.1.0     '
    ACTION

    when ENTER perform COMIENZA-SISTEMA.
loop.

COMIENZA-SISTEMA.
;----------------
    move 1 to MIN-INDICE
    move 5 to MAX-INDICE
    if INDICE >= 1 and INDICE <= 3
       message 1 1 '*MENU PRINCIPAL*'
       message 1 2 '    FACTURAR    '
       message 1 3 '    CANJEAR     '
       message 1 4 ' ABASTECIMIENTO '
    end-if
    if INDICE >= 4 and INDICE <= 5
       message 1 1 '*MENU PRINCIPAL*'
       message 1 2 '                '
       message 1 3 ' CLIENTE  NUEVO '
       message 1 4 '   SUPERVISOR   '
    end-if
    examine INDICE
    when 1 message  1  2 '>'
           message 16  2 '<'
           message  1  3 ' '
           message 16  3 ' '
           message  1  4 ' '
           message 16  4 ' '
    end-when

    when 2 message  1  2 ' '
           message 16  2 ' '
           message  1  3 '>'
           message 16  3 '<'
           message  1  4 ' '
           message  16 4 ' '
    end-when

    when 3 message  1  2 ' '
           message 16  2 ' '
           message  1  3 ' '
           message 16  3 ' '
           message  1  4 '>'
           message 16  4 '<'
    end-when

    when 4 message  1  2 ' '
           message 16  2 ' '
           message  1  3 '>'
           message 16  3 '<'
           message  1  4 ' '
           message 16  4 ' '
    end-when

    when 5 message  1  2 ' '
           message 16  2 ' '
           message  1  3 ' '
           message 16  3 ' '
           message  1  4 '>'
           message 16  4 '<'
    end-when

    FAST-ACTION

    when FAR perform AVA-INDICE.
    when UAR perform AVA-INDICE.
    when BAR perform RET-INDICE.
    when DAR perform RET-INDICE.
    when KEND  exit.
    when ENTER move INDICE to IND-MENU1
               perform OPC-PRINCIPAL
               move IND-MENU1 to INDICE
               move 1 to IND-PANTALLA.
loop.

OPC-PRINCIPAL.
;-------------
    perform ABRE-REF-IDENTIFI
    when ER if IND-MENU1 = 5
               perform ACEP-PASSWORD
            end-if
    end-when
    when OK seek FIRST in IDENTIFI-F
            when OK read IDENTIFI-F

                    move IDE-PASSWO to W-IDE-PASSWO   ; Datos de Identificacion
                    move IDE-BODEGA to W-IDE-BODEGA
                    move IDE-PJ-IVA to W-IDE-IVA
                    move IDE-PJ-ILA to W-IDE-ILA
                    move IDE-NOMBRE to W-IDE-NOMBRE
                    move IDE-VENDED to W-IDE-VENDED
                    move IDE-VISITA to W-IDE-VISITA
                    move IDE-POR-IN to W-IDE-BASE
                    move IDE-FAC-IN to WW-IDE-FAC-IN
                    move IDE-FAC-FI to WW-IDE-FAC-FI

                    examine IND-MENU1
                       when 1 move 1 to INDICE
                              move 1 to IND-PANTALLA
                              perform PRN-VENTAS
                       end-when
                       when 2 perform ABRE-REF-EXISTENC
                              perform ABRE-REF-PRODUCTO
                              when OK perform PRN-CANJEAR.
                       end-when
                       when 3 perform ABRE-REF-SO-ABAST
                              when ER perform ABRE-UPD-SO-ABAST
                                      perform ABRE-REF-EXISTENC
                                      perform ABRE-REF-PRODUCTO
                                      when OK seek FIRST in PRODUCTO-F
                                              when OK message 1 1 '****************'
                                                      message 1 2 ' Generacion en  '
                                                      message 1 3 '   Proceso...   '
                                                      message 1 4 '****************'

                                                      perform PRN-ABASTECIMIENTO

                                                      beep
                                                      message 1 1 '****************'
                                                      message 1 2 '   Solicitud    '
                                                      message 1 3 '    Generada    '
                                                      message 1 4 '****************'
                                                      wait 1 second
                                              end-when
                                      end-when
                              end-when
                              when OK beep
                                      message 1 1 '****************'
                                      message 1 2 '   Solicitud    '
                                      message 1 3 '  YA GENERADA   '
                                      message 1 4 '****************'
                                      wait 1 second

                                      perform GENERA-DE-NUEVO

                              end-when
                       end-when
                       when 4 perform PRN-CLIENTES.
                       when 5 perform ACEP-PASSWORD.
            end-when
    end-when
exit.

PRN-VENTAS.
;----------
    move 1 to MIN-INDICE
    move 5 to MAX-INDICE
    if INDICE >= 1 and INDICE <= 3
       message 1 1 ' *  Facturar  * '
       message 1 2 '  Ingresar FAC. '
       message 1 3 '  Modificar FAC.'
       message 1 4 '  Eliminar FAC. '
    end-if
    if INDICE >= 4 and INDICE <= 5
       message 1 1 ' *  Facturar  * '
       message 1 2 '                '
       message 1 3 '  Anular FAC.   '
       message 1 4 '  Imprimir FAC. '
    end-if
    examine INDICE
    when 1 message 1 2 '>'
           message 1 3 ' '
           message 1 4 ' '.

    when 2 message 1 2 ' '
           message 1 3 '>'
           message 1 4 ' '.

    when 3 message 1 2 ' '
           message 1 3 ' '
           message 1 4 '>'.

    when 4 message 1 2 ' '
           message 1 3 '>'
           message 1 4 ' '.

    when 5 message 1 2 ' '
           message 1 3 ' '
           message 1 4 '>'.

    FAST-ACTION

    when FAR perform AVA-INDICE.
    when UAR perform AVA-INDICE.
    when BAR perform RET-INDICE.
    when DAR perform RET-INDICE.
    when KEND  exit.
    when ENTER move INDICE to IND-MENU2
               perform OPC-VENTAS
               move IND-MENU2 to INDICE
               move 1 to IND-PANTALLA.
loop.

OPC-VENTAS.
;----------
    examine IND-MENU2
       when 1 perform ABRE-REF-IDENTIFI
              when ER beep
                      message 1 1 '****************'
                      message 1 1 '     No hay     '
                      message 1 1 ' IDENTIFICACION '
                      message 1 1 '****************'
              end-when
              when OK seek FIRST in IDENTIFI-F
                      when OK read IDENTIFI-F
                              move IDE-PASSWO to W-IDE-PASSWO
                              move IDE-BODEGA to W-IDE-BODEGA
                              move IDE-PJ-IVA to W-IDE-IVA
                              move IDE-PJ-ILA to W-IDE-ILA
                              move IDE-FAC-IN to WW-IDE-FAC-IN
                              move IDE-FAC-FI to WW-IDE-FAC-FI

                              if IDE-FAC-EM = 0                 ;No se ha Ingresado ninguna Factura Todav¡a
                                 move IDE-FAC-IN to W-IDE-FACTUR
                              else
                                 move IDE-FAC-EM to W-IDE-FACTUR
                              end-if
                              move 1 to MODO-OPERACION
                              perform VENT-INGRESO
                      end-when
              end-when
       end-when
       when 2 move 2 to MODO-OPERACION
              perform EJECUT-OPC-MODIFI
       end-when
       when 3 perform EJECUT-OPC-ELIMIN.
       when 4 perform EJECUT-OPC-ANULAR.
       when 5 perform EJECUT-OPC-IMPRIM.
exit.

EJECUT-OPC-MODIFI.
;-----------------
    perform ABRE-REF-CABECERA
    when OK seek FIRST in CABECERA-F
            when EOF beep
                     message 1 1 '****************'
                     message 1 2 '   No Existen   '
                     message 1 3 '     VENTAS     '
                     message 1 4 '****************'
                     wait 1 second
            end-when
            when OK clear

                    move ' ' to AUX-BLANCO
                    search FIRST for AUX-BLANCO in CABECERA-F offset CAB-FAC-IMP  ;S¢lo Busca las Cabeceras no Impresas
                    when ER beep
                            message 1 1 '****************'
                            message 1 2 'No hay  Facturas'
                            message 1 3 ' para MODIFICAR '
                            message 1 4 '****************'
                            wait 1 second
                    end-when
                    when OK read CABECERA-F
                            perform VENT-MODIFICA
                    end-when
            end-when
    end-when
    when ER beep
            message 1 1 '****************'
            message 1 2 '   No Existen   '
            message 1 3 '     VENTAS     '
            message 1 4 '****************'
            wait 1 second
    end-when
exit.

;ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
;º                   Inicio M¢dulo para MODIFICAR la FACTURA                   º
;ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

VENT-MODIFICA.
;-------------
    move CAB-FACTURA to IMP-CAB-FAC
    move CAB-FACTURA to W-IDE-FACTUR
    move CAB-LOC-CLI to IMP-CAB-LOC
    move CAB-RUT-CLI to IMP-CAB-RUT
    move CAB-RUT-DIG to IMP-CAB-DIG
    move CAB-TOT-FAC to IMP-CAB-TOT
    move CAB-FEC-FAC to AUX-FECHA-FILE

    perform ACCESAR-CLIENTE

    show IMP-CAB-FACTURA-sd
    ACTION

    when ENTER if CAB-FAC-IMP <> ' '
                  beep
                  message 1 1 '****************'
                  message 1 2 '    Factura     '
                  message 1 3 '   YA IMPRESA   '
                  message 1 4 '****************'
                  wait 1 second
               else
                  if CAB-FAC-NUL <> ' '
                     beep
                     message 1 1 '****************'
                     message 1 2 '    Factura     '
                     message 1 3 '      NULA      '
                     message 1 4 '****************'
                     wait 1 second
                  else
                     move 1 to INDICE
                     move 1 to IND-PANTALLA
                     perform DECIDE-LA-OPERACION
                     exit
                  end-if
               end-if
    end-when

    when BAR    perform LOOP-ARRIBA.
    when FAR    perform LOOP-ABAJO.

    when KEND  move KEND to LAST-KEY, exit.
loop.

DECIDE-LA-OPERACION.
;-------------------
    move 1 to MIN-INDICE
    move 2 to MAX-INDICE
    if IND-PANTALLA > 0
       message 1 1 ' *  Modificar * '
       message 1 2 '                '
       message 1 3 '  Agregar Produ '
       message 1 4 '  Mod/Eli Produ '
       move 1 to IND-PANTALLA
    end-if
    examine INDICE

    when 1 message 1 2 ' '
           message 1 3 '>'
           message 1 4 ' '.

    when 2 message 1 2 ' '
           message 1 3 ' '
           message 1 4 '>'.

    FAST-ACTION

    when FAR perform AVA-INDICE.
    when UAR perform AVA-INDICE.
    when BAR perform RET-INDICE.
    when DAR perform RET-INDICE.
    when KEND  exit.
    when ENTER move INDICE to IND-MENU9
               perform OPC-MODIFICAION
               when ER exit.
               move IND-MENU9 to INDICE
               move 1 to IND-PANTALLA
    end-when
loop.

OPC-MODIFICAION.
;---------------
    clear
    move 0 to AUX-LINEAS-MAX
    perform ABRE-UPD-DETALLES
    perform ABRE-REF-EXISTENC
    when OK perform ABRE-REF-PRODUCTO
            when OK move 0 to RE-WW-TOTAL-ILA   ; Incializo TOTAL de RECALCULO en 0
                    search FIRST for IMP-CAB-FAC in DETALLES-F offset DET-FACTURA
                    perform SUMA-LINEAS-DETALLE

                    perform EXAMINE-IND-MENU9
            end-when
    end-when
exit.

EXAMINE-IND-MENU9.
;-----------------
    examine  IND-MENU9
       when 1 move CAB-VAL-IVA     to   W-TOTAL-IVA
              move CAB-VAL-ILA     to   W-TOTAL-ILA
              move CAB-TOT-FAC     to   W-TOT-FACT

              move     CAB-TOT-FAC to   RE-WW-TOTAL-FAC
              subtract CAB-VAL-IVA from RE-WW-TOTAL-FAC
              subtract CAB-VAL-ILA from RE-WW-TOTAL-FAC


              perform ABRE-REF-PRODUCTO
              when OK seek FIRST in PRODUCTO-F
                      perform ACEP-COD-PRODUCTO
              end-when
       end-when
       when 2 move CAB-VAL-IVA     to   W-TOTAL-IVA
              move CAB-VAL-ILA     to   W-TOTAL-ILA
              move CAB-TOT-FAC     to   W-TOT-FACT

              move     CAB-TOT-FAC to   RE-WW-TOTAL-FAC
              subtract CAB-VAL-IVA from RE-WW-TOTAL-FAC
              subtract CAB-VAL-ILA from RE-WW-TOTAL-FAC

              perform MOD-ELI-PRODUCTOS
       end-when
exit.

MOD-ELI-PRODUCTOS.
;-----------------
    search FIRST for IMP-CAB-FAC in DETALLES-F offset DET-FACTURA
    when ER beep
            message 1 1 '****************'
            message 1 2 '   No Existe    '
            message 1 3 '    DETALLE     '
            message 1 4 '****************'
            wait 1 second
    end-when
    when OK read DETALLES-F
            clear
            perform LOOP-DET-MODIFICA
    end-when
exit.

LOOP-DET-MODIFICA.
;-----------------
    move DET-PRODUCT to W-PRD-CODIGO
    move DET-CAN-UNI to W-PRD-CANTID
    move DET-CAN-UNI to W-PRD-CAN-AN

    search FIRST for W-PRD-CODIGO in PRODUCTO-F offset PRD-CODIGO
    when ER move 'Sin Descripcion' to W-PRD-DESCRI.
    when OK read PRODUCTO-F
            move PRD-DESCRI to W-PRD-DESCRI
            move PRD-DESCRI to W-PRD-DESCRI-9
            move PRD-UN-BAS to W-PR-UN-BAS
            move PRD-PR-VTA to W-PR-UN-VTA
            move PRD-DCT-PR to WW-DCT-PRODU
    end-when

    move W-IDE-BODEGA to AUX-LL1-BOD
    move DET-PRODUCT  to AUX-LL1-PRD

    search FIRST for AUX-LL1-EX-BODEGA in EX-BODEG-F offset EXI-LL1
    when ER move 0          to W-PRO-STOCK.
    when OK read EX-BODEG-F
            move EXI-SALDOO to W-PRO-STOCK
    end-when

    show  W-PRD-CODIG2-sd
    show  W-PRD-DESCRI-sd
    show  W-PRD-CANTID-sd

    FAST-ACTION

    when KDEL  perform ELIMINAR-UN-ITEM
               clear
               when ER exit.
    end-when
    when CLR   move CAB-VAL-IVA to W-TOTAL-IVA
               move CAB-VAL-ILA to W-TOTAL-ILA
               move CAB-TOT-FAC to W-TOT-FACT

               perform CALC-UNIDAD-BASE
               perform CALC-PRECIO-VENTA

               show W-COD-DES-PROD-sd
               show W-PRECIO-UB-sd
               show W-PRECIO-UV-sd

               perform PEDIR-NUEVA-CANTIDAD
               exit
    end-when
    when KEND  exit.
    when FAR   perform AVANZA-DETALLE.
    when BAR   perform RETROCEDE-DETALLE.
loop.

AVANZA-DETALLE.
;-------------
    seek NEXT in DETALLES-F
    when EOF
         beep
         message 1 4 ' Ultimo Detalle '
         wait 1 SECOND
         clear 4
         seek LAST in DETALLES-F
    end-when
    read DETALLES-F
    if CAB-FACTURA <> DET-FACTURA
       beep
       message 1 4 ' Ultimo Detalle '
       wait 1 second
       clear 4
       seek PRIOR in DETALLES-F
       read DETALLES-F
    end-if
exit.

RETROCEDE-DETALLE.
;----------------
    seek PRIOR in DETALLES-F
    when EOF
         beep
         message 1 4 'Primer   Detalle'
         wait 1 second
         clear 4
         seek FIRST in DETALLES-F
    end-when
    read DETALLES-F
    if CAB-FACTURA <> DET-FACTURA
       beep
       message 1 4 'Primer   Detalle'
       wait 1 second
       clear 4
       seek NEXT in DETALLES-F
       read DETALLES-F
    end-if
exit.

ELIMINAR-UN-ITEM.
;----------------
    clear
    message 1 1 '    Elimina     '
    message 1 2 '  Item/DETALLE  '
    message 1 3 '****************'
    message 1 4 '   [SI]  [NO]   '
    ACTION

    when NO    exit.
    when ENTER move 0 to W-TOTAL-IVA
               move 0 to W-TOTAL-ILA
               move 0 to W-TOT-FACT

               perform CALC-UNIDAD-BASE
               perform CALC-PRECIO-VENTA

               move     W-PRECIO-UB  to W-TOT-PROD
               multiply W-TOT-PROD   by W-PRD-CANTID
               perform  VER-SI-ES-ILA

               perform  BORRAR-ITEM
               exit
    end-when
loop.

BORRAR-ITEM.
;----------
    add W-PRD-CANTID to EXI-SALDOO
    rewrite EX-BODEG-R

    move DET-FACTURA to AUX-FACTURA
    DELETE RECORD from DETALLES-F

    search FIRST for AUX-FACTURA in DETALLES-F offset DET-FACTURA
    when ER perform ABRE-UPD-CABECERA
            search FIRST for AUX-FACTURA in CABECERA-F offset CAB-FACTURA
            read CABECERA-F

            DELETE RECORD from CABECERA-F
            move ER to LAST-KEY
    end-when
    when OK read DETALLES-F

            perform ABRE-REF-CABECERA
            search FIRST for AUX-FACTURA in CABECERA-F offset CAB-FACTURA
            read CABECERA-F

            subtract W-TOTAL-IVA from CAB-VAL-IVA
            subtract W-TOTAL-ILA from CAB-VAL-ILA
            subtract W-TOT-FACT  from CAB-TOT-FAC

            rewrite CABECERA-R

            move OK to LAST-KEY
    end-when
exit.

PEDIR-NUEVA-CANTIDAD.
;-------------------
    accept W-PRD-CANTID-SD
    when OK if W-PRD-CANTID = 0 loop.

            add W-PRD-CAN-AN to W-PRO-STOCK

            if W-PRD-CANTID > W-PRO-STOCK

               beep
               message 1 4 '  Insuficiente  '
               wait 1 second
               clear 4
               subtract W-PRD-CAN-AN from W-PRO-STOCK

            else
               perform RE-ACTUALIZAR-STOCK

               move     W-PRECIO-UB   to W-TOT-PROD
               multiply W-TOT-PROD    by W-PRD-CAN-AN ; Cantidad Antigua
               perform  RE-ACTUALIZAR-TOTAL

               move     W-PRECIO-UB   to W-TOT-PROD
               multiply W-TOT-PROD    by W-PRD-CANTID ; Cantidad Nueva
               perform  VER-SI-ES-ILA

               perform  MUESTRA-EL-TOT-PROD
               perform  ACTUALIZAR-DETALLE
               perform  ACTUALIZAR-CABECER
               exit
            end-if


    end-when
    when KEND  exit.
loop.

RE-ACTUALIZAR-STOCK.
;------------------
    add      W-PRD-CAN-AN to   EXI-SALDOO
    subtract W-PRD-CANTID from EXI-SALDOO
    rewrite  EX-BODEG-R
exit.

RE-ACTUALIZAR-TOTAL.
;-------------------
    subtract W-TOT-PROD from RE-WW-TOTAL-FAC

    move     W-TOT-PROD   to   W-TOT-IVA
    multiply W-TOT-IVA    by   W-IDE-IVA
    add      5000         to   W-TOT-IVA
    divide   10000        into W-TOT-IVA
    subtract W-TOT-IVA    from W-TOT-FACT

    subtract W-TOT-IVA    from W-TOTAL-IVA

    if W-PRD-UNO > 2749 and W-PRD-UNO < 2752
       subtract W-TOT-PROD   from RE-WW-TOTAL-ILA

       move     W-TOT-PROD   to   W-TOT-ILA
       multiply W-TOT-ILA    by   W-IDE-ILA
       add      5000         to   W-TOT-ILA
       divide   10000        into W-TOT-ILA
       subtract W-TOT-ILA    from W-TOT-FACT

       subtract W-TOT-ILA    from W-TOTAL-ILA
    end-if

    subtract W-TOT-PROD from W-TOT-FACT

exit.

ACTUALIZAR-DETALLE.
;-----------------
    perform CALCULA-EL-DETALLE

    move W-PRD-CANTID     to DET-CAN-UNI
    move CALC-VAL-BRU-PRD to DET-VAL-BRU
    move WW-DCT-PRODU     to DET-DES-PRO
    move CALC-VAL-DCT-PRM to DET-VAL-PRO
    move WW-DCT-CLIEN     to DET-DES-BAS
    move CALC-VAL-DCT-BAS to DET-VAL-BAS

    rewrite DETALLES-R
exit.

ACTUALIZAR-CABECER.
;------------------
    move     RE-WW-TOTAL-FAC to   RE-WW-TOTAL-IVA
    multiply RE-WW-TOTAL-IVA by   W-IDE-IVA
    add      5000            to   RE-WW-TOTAL-IVA
    divide   10000           into RE-WW-TOTAL-IVA
    add      RE-WW-TOTAL-IVA to   RE-WW-TOTAL-FAC

    multiply RE-WW-TOTAL-ILA by   W-IDE-ILA
    add      5000            to   RE-WW-TOTAL-ILA
    divide   10000           into RE-WW-TOTAL-ILA
    add      RE-WW-TOTAL-ILA to   RE-WW-TOTAL-FAC

    move RE-WW-TOTAL-IVA     to CAB-VAL-IVA
    move RE-WW-TOTAL-ILA     to CAB-VAL-ILA
    move RE-WW-TOTAL-FAC     to CAB-TOT-FAC

    rewrite CABECERA-R
exit.

SUMA-LINEAS-DETALLE.
;-------------------
    read DETALLES-F
    add 1 to AUX-LINEAS-MAX

    move DET-PRODUCT to W-PRD-CODIGO

    if W-PRD-UNO > 2749 and W-PRD-UNO < 2752
       add DET-VAL-BRU to RE-WW-TOTAL-ILA
    end-if

    search NEXT for IMP-CAB-FAC in DETALLES-F offset DET-FACTURA
    when ER exit.
loop.

;ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
;º                  T‚rmino M¢dulo para MODIFICAR la FACTURA                   º
;ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

;ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
;º                   Inicio M¢dulo para ELIMINAR  la FACTURA                   º
;ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

EJECUT-OPC-ELIMIN.
;-----------------
    perform ABRE-REF-CABECERA
    when OK seek FIRST in CABECERA-F
            when EOF beep
                     message 1 1 '****************'
                     message 1 2 '   No Existen   '
                     message 1 3 '     VENTAS     '
                     message 1 4 '****************'
                     wait 1 second
            end-when
            when OK clear
                    move ' ' to AUX-BLANCO
                    search FIRST for AUX-BLANCO in CABECERA-F offset CAB-FAC-IMP
                    when ER beep
                            message 1 1 '****************'
                            message 1 2 'No hay  Facturas'
                            message 1 3 ' para ELIMINAR  '
                            message 1 4 '****************'
                            wait 1 second
                    end-when
                    when OK read CABECERA-F
                            perform VENT-ELIMINAR
                    end-when
            end-when
    end-when
    when ER beep
            message 1 1 '****************'
            message 1 2 '   No Existen   '
            message 1 3 '     VENTAS     '
            message 1 4 '****************'
            wait 1 second
    end-when
exit.

VENT-ELIMINAR.
;-------------
    move CAB-FACTURA to IMP-CAB-FAC
    move CAB-LOC-CLI to IMP-CAB-LOC
    move CAB-RUT-CLI to IMP-CAB-RUT
    move CAB-RUT-DIG to IMP-CAB-DIG
    move CAB-TOT-FAC to IMP-CAB-TOT
    move CAB-FEC-FAC to AUX-FECHA-FILE

    perform ACCESAR-CLIENTE

    show IMP-CAB-FACTURA-sd
    FAST-ACTION

    when ENTER if CAB-FAC-IMP <> ' ' or CAB-FAC-NUL <> ' '
                  beep
                  message 1 1 '****************'
                  message 1 2 '  Factura esta  '
                  message 1 3 ' IMPRESA o NULA '
                  message 1 4 '****************'
                  wait 1 second
                  clear
               else
                  perform ELIMINAR-FACTURA
                  exit
               end-if
    end-when

    when BAR    perform LOOP-ARRIBA.
    when FAR    perform LOOP-ABAJO.

    when KEND  exit.
loop.

ELIMINAR-FACTURA.
;---------------
    clear
    beep
    message 1 1 '    Elimina     '
    message 1 2 '     VENTA      '
    message 1 3 '****************'
    message 1 4 '   [SI]  [NO]   '

    ACTION

    when NO exit.
    when ENTER perform BORRAR-ENCAB, exit.
loop.

BORRAR-ENCAB.
;-----------
    move    IMP-CAB-FAC to AUX-FACTURA
    perform ABRE-REF-EXISTENC
    perform ABRE-UPD-DETALLES
    perform RE-ACT-STOCK-BORRA

    perform ABRE-UPD-CABECERA
    search FIRST for AUX-FACTURA in CABECERA-F offset CAB-FACTURA
    when OK read CABECERA-F
            DELETE RECORD from CABECERA-F

            move ER to LAST-KEY
    end-when
exit.

RE-ACT-STOCK-BORRA.
;------------------
    search FIRST for IMP-CAB-FAC in DETALLES-F offset DET-FACTURA
    when ER exit.
    when OK read DETALLES-F

            move W-IDE-BODEGA to AUX-LL1-BOD
            move DET-PRODUCT  to AUX-LL1-PRD

            search FIRST for AUX-LL1-EX-BODEGA in EX-BODEG-F offset EXI-LL1
            when ER exit.
            when OK read EX-BODEG-F
                    add DET-CAN-UNI to EXI-SALDOO
                    rewrite EX-BODEG-R

                    DELETE RECORD from DETALLES-F
            end-when
    end-when
loop.

;ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
;º                  T‚rmino M¢dulo para ELIMINAR  la FACTURA                   º
;ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

EJECUT-OPC-ANULAR.
;-----------------
    perform ABRE-REF-CABECERA
    when OK seek FIRST in CABECERA-F
            when EOF beep
                     message 1 1 '****************'
                     message 1 2 '   No Existen   '
                     message 1 3 '     VENTAS     '
                     message 1 4 '****************'
                     wait 1 second
            end-when
            when OK read CABECERA-F
                    clear
                    perform VENT-ANULA
            end-when
    end-when
    when ER beep
            message 1 1 '****************'
            message 1 2 '   No Existen   '
            message 1 3 '     VENTAS     '
            message 1 4 '****************'
            wait 1 second
    end-when
exit.

VENT-ANULA.
;----------
    move CAB-FACTURA to IMP-CAB-FAC
    move CAB-LOC-CLI to IMP-CAB-LOC
    move CAB-RUT-RUT to IMP-CAB-RUT
    move CAB-RUT-DIG to IMP-CAB-DIG
    move CAB-TOT-FAC to IMP-CAB-TOT
    move CAB-FEC-FAC to AUX-FECHA-FILE

    perform ACCESAR-CLIENTE

    show IMP-CAB-FACTURA-sd
    FAST-ACTION

    when BAR    seek PRIOR in CABECERA-F
                when EOF beep
                         message 1 4 'Primera Factura '
                         wait 1 second
                         seek FIRST in CABECERA-F
                end-when
                read CABECERA-F
    end-when
    when FAR    seek NEXT in CABECERA-F
                when EOF beep
                         message 1 4 ' Ultima Factura '
                         wait 1 second
                         seek LAST in CABECERA-F
                end-when
                read CABECERA-F
    end-when
    when ENTER  if CAB-FAC-NUL <> ' '
                   beep
                   message 1 4 '    ANULADA     '
                   wait 1 second
                else
                   if CAB-FAC-IMP <> ' '

                      beep
                      message 1 2 ' Esta Seguro ?  '
                      message 1 3 '****************'
                      message 1 4 '   [SI]  [NO]   '

                      ACTION

                      when ENTER move 'X' to CAB-FAC-NUL
                                 rewrite CABECERA-R

                                 perform ABRE-REF-DETALLES
                                 perform ABRE-REF-EXISTENC

                                 search FIRST for IMP-CAB-FAC in DETALLES-F offset DET-FACTURA
                                 when OK perform DEVUELVE-STOCK-ANULA.

                                 beep
                                 message 1 1 '****************'
                                 message 1 2 '    Factura     '
                                 message 1 3 '    ANULADA     '
                                 message 1 4 '****************'
                                 wait 1 second
                      end-when
                   else
                      beep
                      message 1 1 '****************'
                      message 1 2 ' Factura no ha  '
                      message 1 3 '  sido IMPRESA  '
                      message 1 4 '****************'
                      wait 1 second
                   end-if
                end-if
    end-when
    when KEND   exit.
loop.

DEVUELVE-STOCK-ANULA.
;--------------------
    read DETALLES-F

    move W-IDE-BODEGA to AUX-LL1-BOD
    move DET-PRODUCT  to AUX-LL1-PRD

    search FIRST for AUX-LL1-EX-BODEGA in EX-BODEG-F offset EXI-LL1
    when OK read EX-BODEG-F
            move EXI-SALDOO  to W-PRO-STOCK
            add  DET-CAN-UNI to W-PRO-STOCK
            move W-PRO-STOCK to EXI-SALDOO
            rewrite EX-BODEG-R
    end-when

    search NEXT for IMP-CAB-FAC in DETALLES-F offset DET-FACTURA
    when ER exit.

loop.

ACCESAR-CLIENTE.
;--------------
    perform ABRE-REF-CLI-NUEV
    perform ABRE-REF-CLI-RUTA
    perform ABRE-REF-CLI-CART

    move CAB-RUT-RUT to W-CL-N-RUT
    move CAB-RUT-DIG to W-CL-N-DIG

    search FIRST for CAB-LL2 in CL-CARTE-F offset CL-C-LL1
    when ER
            search FIRST for CAB-LL2 in CL-RUTA-F offset CL-R-LL1
            when ER
                    search FIRST for CAB-LL2 in CL-NUEVO-F offset CL-N-LL1
                    when ER beep
                            message 1 1 '****************'
                            message 1 2 ' No  Encontrado '
                            message 1 3 '................'
                            message 1 4 '****************'
                            wait 1 second
                            clear
                    end-when
                    when OK read CL-NUEVO-F
                            move CL-N-RAZON  to IMP-CAB-NOM
                            move CL-N-RAZON  to IMP-CAB-RAZ
                            move CL-N-DESPAC to IMP-CAB-DES
                            move CL-N-CIUDAD to IMP-CAB-CIU
                            move CL-N-DCT-BA to WW-DCT-CLIEN
                            exit
                    end-when
            end-when
            when OK read CL-RUTA-F
                    move CL-R-RAZON  to IMP-CAB-NOM
                    move CL-R-RAZON  to IMP-CAB-RAZ
                    move CL-R-DESPAC to IMP-CAB-DES
                    move CL-R-CIUDAD to IMP-CAB-CIU
                    move CL-R-DCT-BA to WW-DCT-CLIEN
            end-when
    end-when
    when OK read CL-CARTE-F
            move CL-C-RAZON  to IMP-CAB-NOM
            move CL-C-RAZON  to IMP-CAB-RAZ
            move CL-C-DESPAC to IMP-CAB-DES
            move CL-C-CIUDAD to IMP-CAB-CIU
            move CL-C-DCT-BA to WW-DCT-CLIEN
    end-when
exit.

EJECUT-OPC-IMPRIM.
;-----------------
    perform ABRE-REF-CABECERA
    when OK seek FIRST in CABECERA-F
            when EOF beep
                     message 1 1 '****************'
                     message 1 2 '   No Existen   '
                     message 1 3 '     VENTAS     '
                     message 1 4 '****************'
                     wait 1 second
            end-when
            when OK  read CABECERA-F
                     clear
                     perform ABRE-REF-PRODUCTO
                     perform ABRE-REF-DETALLES

                     move ' ' to AUX-BLANCO
                     search FIRST for AUX-BLANCO in CABECERA-F offset CAB-FAC-IMP
                     when ER beep
                             message 1 1 '****************'
                             message 1 2 'No hay  Facturas'
                             message 1 3 ' para IMPRIMIR  '
                             message 1 4 '****************'
                             wait 1 second
                     end-when
                     when OK read CABECERA-F
                             perform VENT-IMPRIME
                     end-when
            end-when
    end-when
    when ER beep
            message 1 1 '****************'
            message 1 2 '   No Existen   '
            message 1 3 '     VENTAS     '
            message 1 4 '****************'
            wait 1 second
    end-when
exit.

VENT-INGRESO.
;------------
    clear
    show W-IDE-FACTUR-sd
    action

    when CLR   move W-IDE-FACTUR to WW-AUX-FACTUR
               perform ACEP-NRO-FACTURA
    end-when
    when KEND  exit.
    when ENTER perform ABRE-REF-CABECERA
               when ER perform SEL-TIPO-CLIENTE.
               when OK perform SI-YA-FUE-EMITIDA
                       when ER perform SEL-TIPO-CLIENTE.
               end-when
               exit
    end-when
loop.

ACEP-NRO-FACTURA.
;----------------
    accept W-IDE-FACTUR-sd
    when OK if W-IDE-FACTUR = 0, loop.

            if W-IDE-FACTUR < WW-IDE-FAC-IN or W-IDE-FACTUR > WW-IDE-FAC-FI
               alarm
               message 1 1 '****************'
               message 1 2 'Folio de Factura'
               message 1 3 ' fuera de Rango '
               message 1 4 '****************'
               wait 1 second
               clear
               move WW-AUX-FACTUR to W-IDE-FACTUR
            else
               exit
            end-if
    end-when
    when KEND exit.
loop.

SI-YA-FUE-EMITIDA.
;-----------------
    search FIRST for W-IDE-FACTUR in CABECERA-F offset CAB-FACTURA
    when OK beep
            message 1 1 '****************'
            message 1 2 '   Factura ya   '
            message 1 3 '    Emitida     '
            message 1 4 '****************'
            wait 1 second
    end-when
exit.

SEL-TIPO-CLIENTE.
;----------------
    move 1 to MIN-INDICE
    move 3 to MAX-INDICE
    if IND-PANTALLA > 0

       show W-IDE-FACTUR-sd

       message 1 2 '  Cliente Ruta  '
       message 1 3 '  Cliente Cart. '
       message 1 4 '  Cliente Nuev. '

       move 0 to IND-PANTALLA
    end-if
    examine INDICE

    when 1 message 1 2 '>'
           message 1 3 ' '
           message 1 4 ' '.

    when 2 message 1 2 ' '
           message 1 3 '>'
           message 1 4 ' '.

    when 3 message 1 2 ' '
           message 1 3 ' '
           message 1 4 '>'.

    FAST-ACTION

    when FAR perform AVA-INDICE.
    when UAR perform AVA-INDICE.
    when BAR perform RET-INDICE.
    when DAR perform RET-INDICE.
    when KEND  exit.
    when ENTER move INDICE to IND-MENU9
               perform OPC-TIPO-CLIENTE
               move IND-MENU9 to INDICE
               move 1 to IND-PANTALLA
    end-when
loop.

OPC-TIPO-CLIENTE.
;----------------
    clear
    examine IND-MENU9
       when 1 perform VA-PARA-RUTA.
       when 2 perform VA-PARA-CARTERA.
       when 3 perform VA-PARA-NUEVO.
exit.

VA-PARA-RUTA.
;------------
    perform ABRE-REF-EXISTENC
    perform ABRE-REF-CONDICIO

    perform ABRE-REF-CLI-RUTA
    when ER beep
            message 1 1 '****************'
            message 1 2 '     No hay     '
            message 1 3 ' CLIENTES RUTA  '
            message 1 4 '****************'
            wait 1 second
    end-when
    when OK perform RUT-TIP-CLI-RUTA.
exit.

VA-PARA-CARTERA.
;---------------
    perform ABRE-REF-EXISTENC
    perform ABRE-REF-CONDICIO

    perform ABRE-REF-CLI-CART
    when ER beep
            message 1 1 '****************'
            message 1 2 '     No hay     '
            message 1 3 'CLIENTES CARTERA'
            message 1 4 '****************'
            wait 1 second
    end-when
    when OK perform RUT-TIP-CLI-CARTERA.
exit.

VA-PARA-NUEVO.
;-------------
    perform ABRE-REF-EXISTENC
    perform ABRE-REF-CONDICIO

    perform ABRE-REF-CLI-NUEV
    when ER beep
            message 1 1 '****************'
            message 1 2 '     No hay     '
            message 1 3 'CLIENTES  NUEVOS'
            message 1 4 '****************'
            wait 1 second
    end-when
    when OK seek FIRST in CL-NUEVO-F
            when EOF beep
                     message 1 1 '****************'
                     message 1 2 '     No hay     '
                     message 1 3 'CLIENTES  NUEVOS'
                     message 1 4 '****************'
                     wait 1 second
            end-when
            when OK perform RUT-TIP-CLI-NUEVO.
    end-when
exit.

RUT-TIP-CLI-RUTA.
;----------------
    show W-IDE-FACTUR-sd
    accept ING-CLI-RUT-sd
    when OK if W-EL-RUT = 0
               clear

               seek FIRST in CL-RUTA-F
               when OK read CL-RUTA-F
                       move 0 to WW-CON-VTA-CAB
                       perform MUESTRA-CLI-RUTA
                       exit
               end-when
            else
               perform PIDE-EL-DIGITO
            end-if
    end-when
    when KEND exit.
    when BAR  clear, seek FIRST in CL-RUTA-F, read CL-RUTA-F, move 0 to WW-CON-VTA-CAB, perform MUESTRA-CLI-RUTA, exit.
    when FAR  clear, seek FIRST in CL-RUTA-F, read CL-RUTA-F, move 0 to WW-CON-VTA-CAB, perform MUESTRA-CLI-RUTA, exit.
loop.

RUT-TIP-CLI-CARTERA.
;-------------------
    show W-IDE-FACTUR-sd
    clear 2 - 4
    accept ING-CLI-RUT-sd
    when OK if W-EL-RUT = 0
               clear

               seek FIRST in CL-CARTE-F
               when OK read CL-CARTE-F
                       move 0 to WW-CON-VTA-CAB
                       perform MUESTRA-CLI-CARTERA
                       exit
               end-when
            else
               perform PIDE-EL-DIGITO
            end-if
    end-when
    when KEND exit.
    when BAR  clear, seek FIRST in CL-CARTE-F, read CL-CARTE-F, move 0 to WW-CON-VTA-CAB, perform MUESTRA-CLI-CARTERA, exit.
    when FAR  clear, seek FIRST in CL-CARTE-F, read CL-CARTE-F, move 0 to WW-CON-VTA-CAB, perform MUESTRA-CLI-CARTERA, exit.
loop.

RUT-TIP-CLI-NUEVO.
;-----------------
    show W-IDE-FACTUR-sd
    clear 2 - 4
    accept ING-CLI-RUT-sd
    when OK if W-EL-RUT = 0
               clear

               seek FIRST in CL-NUEVO-F
               when OK read CL-NUEVO-F
                       move 0 to WW-CON-VTA-CAB
                       perform MUESTRA-CLI-NUEVO
                       exit
               end-when
            else
               perform PIDE-EL-DIGITO
            end-if
    end-when
    when KEND exit.
    when BAR  clear, seek FIRST in CL-NUEVO-F, read CL-NUEVO-F, move 0 to WW-CON-VTA-CAB, perform MUESTRA-CLI-NUEVO, exit.
    when FAR  clear, seek FIRST in CL-NUEVO-F, read CL-NUEVO-F, move 0 to WW-CON-VTA-CAB, perform MUESTRA-CLI-NUEVO, exit.
loop.

PIDE-EL-DIGITO.
;--------------
    accept ING-CLI-DIG-SD
    when OK if W-EL-DIG = ' ', loop.

            move W-EL-RUT   to RUT-1
            move 2          to WS-CON
            move 8          to WS-TBL
            move 0          to WS-SUMA
            move 0          to RESTO-AUX1
            perform VALIDA-EL-DIGITO

            if WS-DIG = W-EL-DIG
               perform PIDE-LA-SUCURSAL
               exit
            else
               beep
               message 1 4 'Digito  Invalido'
               wait 1 second
               message 1 4 '                '
            end-if
    end-when
    when KEND exit.
loop.

PIDE-LA-SUCURSAL.
;----------------
    accept ING-CLI-COD-sd
    when OK if IND-MENU9 = 1    ; Cliente Ruta
               perform ABRE-REF-CLI-RUTA
               search FIRST for W-CL-C-LL1 in CL-RUTA-F offset CL-R-LL1
               when ER beep
                       message 1 4 '   No Existe    '
                       wait 1 second
                       clear 3 - 4
               end-when
               when OK clear 2 - 4
                       read CL-RUTA-F
                       move 0 to WW-CON-VTA-CAB
                       perform MUESTRA-CLI-RUTA
               end-when
            end-if

            if IND-MENU9 = 2    ; Cliente Cartera
               perform ABRE-REF-CLI-CART
               search FIRST for W-CL-C-LL1 in CL-CARTE-F offset CL-C-LL1
               when ER beep
                       message 1 4 '   No Existe    '
                       wait 1 second
                       clear 3 - 4
               end-when
               when OK read CL-CARTE-F
                       move 0 to WW-CON-VTA-CAB
                       perform MUESTRA-CLI-CARTERA
               end-when
            end-if

            if IND-MENU9 = 3    ; Cliente Nuevo
               perform ABRE-REF-CLI-NUEV
               search FIRST for W-CL-C-LL1 in CL-NUEVO-F offset CL-N-LL1
               when ER beep
                       message 1 4 '   No Existe    '
                       wait 1 second
                       clear 3 - 4
               end-when
               when OK read CL-NUEVO-F
                       move 0 to WW-CON-VTA-CAB
                       perform MUESTRA-CLI-NUEVO
               end-when
            end-if
            exit
    end-when
    when KEND exit.
loop.

MUESTRA-CLI-RUTA.
;----------------
    show W-IDE-FACTUR-sd
    move CL-R-RUT    to W-EL-RUT
    move CL-R-DIG    to W-EL-DIG
    move CL-R-LOCAL  to W-CL-C-LOCAL
    move CL-R-RAZON  to W-CL-C-RAZON
    move CL-R-RAZON  to W-NEXT-RAZON

    if WW-CON-VTA-CAB <> 0
       move WW-CON-VTA-CAB to W-CL-C-CONDIC
    else
       move CL-R-CONDIC    to W-CL-C-CONDIC
    end-if

    search FIRST for W-CL-C-CONDIC in COND-VTA-F offset CON-CODIGO
    when ER move ' '        to W-NEXT-CONDI
            move 0          to W-NEXT-VENCI
    end-when
    when OK read COND-VTA-F
            move CON-DESCRI to W-NEXT-CONDI
            move CON-PL-VEN to W-NEXT-VENCI
    end-when

    show LOOP-CLI-CARTERA
    FAST-ACTION

    when BAR   move 0 to WW-CON-VTA-CAB
               seek PRIOR in CL-RUTA-F
               when EOF beep
                        message 1 4 ' Primer Cliente '
                        wait 1 second
                        seek FIRST in CL-RUTA-F
               end-when
               when OK move 0 to WW-CON-VTA-CAB
                       read CL-RUTA-F
               end-when
    end-when
    when FAR   move 0 to WW-CON-VTA-CAB
               seek NEXT in CL-RUTA-F
               when EOF beep
                        message 1 4 ' Ultimo Cliente '
                        wait 1 second
                        seek LAST in CL-RUTA-F
               end-when
               when OK move 0 to WW-CON-VTA-CAB
                       read CL-RUTA-F
               end-when
    end-when
    when CLR   perform ACEP-COD-COND-VTA
;              move W-CON-VTA-COD to WW-CON-VTA-CAB
    end-when
    when KNEXT perform NEXT-SOCIAL.
    when KEND  exit.
    when ENTER perform ABRE-REF-PRODUCTO
               when OK move CL-R-LOCAL  to W-CLI-LOCAL
                       move CL-R-RUT    to W-CLI-RUT
                       move CL-R-DIG    to W-CLI-DIG
                       move CL-R-RAZON  to W-CLI-RAZON
                       move CL-R-DESPAC to W-CLI-DESPAC
                       move CL-R-VENDED to W-CLI-VENDED
                       move CL-R-VISITA to W-CLI-VISITA
                       move CL-R-DCT-BA to WW-DCT-CLIEN

                       move 0 to W-TOT-FACT
                       move 0 to W-TOTAL-IVA
                       move 0 to W-TOTAL-ILA

                       move 0 to RE-WW-TOTAL-FAC ; Para Recalcular el IVA
                       move 0 to RE-WW-TOTAL-ILA

                       move 0 to AUX-LINEAS-MAX

                       perform ABRE-UPD-DETALLES
                       perform ABRE-REF-PRODUCTO
                       seek FIRST in PRODUCTO-F
                       perform ACEP-COD-PRODUCTO

                       exit
               end-when
    end-when
loop.

MUESTRA-CLI-CARTERA.
;-------------------
    show W-IDE-FACTUR-sd

    move CL-C-RUT    to W-EL-RUT
    move CL-C-DIG    to W-EL-DIG
    move CL-C-LOCAL  to W-CL-C-LOCAL
    move CL-C-RAZON  to W-CL-C-RAZON
    move CL-C-RAZON  to W-NEXT-RAZON

    if WW-CON-VTA-CAB <> 0
       move WW-CON-VTA-CAB to W-CL-C-CONDIC
    else
       move CL-C-CONDIC    to W-CL-C-CONDIC
    end-if

    search FIRST for W-CL-C-CONDIC in COND-VTA-F offset CON-CODIGO
    when ER move ' '        to W-NEXT-CONDI
            move 0          to W-NEXT-VENCI
    end-when
    when OK read COND-VTA-F
            move CON-DESCRI to W-NEXT-CONDI
            move CON-PL-VEN to W-NEXT-VENCI
    end-when

    show LOOP-CLI-CARTERA

    FAST-ACTION

    when BAR   move 0 to WW-CON-VTA-CAB
               seek PRIOR in CL-CARTE-F
               when EOF beep
                        message 1 4 ' Primer Cliente '
                        wait 1 second
                        clear 4
                        seek FIRST in CL-CARTE-F
               end-when
               read CL-CARTE-F
    end-when
    when FAR   move 0 to WW-CON-VTA-CAB
               seek NEXT in CL-CARTE-F
               when EOF beep
                        message 1 4 ' Ultimo Cliente '
                        wait 1 second
                        clear 4
                        seek LAST in CL-CARTE-F
               end-when
               read CL-CARTE-F
    end-when
    when CLR   perform ACEP-COD-COND-VTA
;              move W-CON-VTA-COD to WW-CON-VTA-CAB
    end-when
    when KNEXT perform NEXT-SOCIAL.
    when KEND  exit.
    when ENTER perform ABRE-REF-PRODUCTO
               when OK move CL-C-LOCAL  to W-CLI-LOCAL
                       move CL-C-RUT    to W-CLI-RUT
                       move CL-C-DIG    to W-CLI-DIG
                       move CL-C-RAZON  to W-CLI-RAZON
                       move CL-C-DESPAC to W-CLI-DESPAC
                       move CL-C-VENDED to W-CLI-VENDED
                       move CL-C-VISITA to W-CLI-VISITA
                       move CL-C-DCT-BA to WW-DCT-CLIEN

                       move 0 to W-TOT-FACT
                       move 0 to W-TOTAL-IVA
                       move 0 to W-TOTAL-ILA
                       move 0 to AUX-LINEAS-MAX

                       move 0 to RE-WW-TOTAL-FAC ; Para Recalcular el IVA
                       move 0 to RE-WW-TOTAL-ILA

                       perform ABRE-UPD-DETALLES
                       perform ABRE-REF-PRODUCTO
                       seek FIRST in PRODUCTO-F
                       perform ACEP-COD-PRODUCTO

                       exit
               end-when
    end-when
loop.

MUESTRA-CLI-NUEVO.
;-----------------
    show W-IDE-FACTUR-sd
    move CL-N-RUT    to W-EL-RUT
    move CL-N-DIG    to W-EL-DIG
    move CL-N-LOCAL  to W-CL-C-LOCAL
    move CL-N-RAZON  to W-CL-C-RAZON
    move CL-N-RAZON  to W-NEXT-RAZON

    if WW-CON-VTA-CAB <> 0
       move WW-CON-VTA-CAB to W-CL-C-CONDIC
    else
       move CL-N-CONDIC    to W-CL-C-CONDIC
    end-if

    search FIRST for W-CL-C-CONDIC in COND-VTA-F offset CON-CODIGO
    when ER move ' '        to W-NEXT-CONDI
            move 0          to W-NEXT-VENCI
    end-when
    when OK read COND-VTA-F
            move CON-DESCRI to W-NEXT-CONDI
            move CON-PL-VEN to W-NEXT-VENCI
    end-when

    show LOOP-CLI-CARTERA
    FAST-ACTION

    when BAR   move 0 to WW-CON-VTA-CAB
               seek PRIOR in CL-NUEVO-F
               when EOF beep
                        message 1 4 ' Primer Cliente '
                        wait 1 second
                        seek FIRST in CL-NUEVO-F
               end-when
               read CL-NUEVO-F
    end-when
    when FAR   move 0 to WW-CON-VTA-CAB
               seek NEXT in CL-NUEVO-F
               when EOF beep
                        message 1 4 ' Ultimo Cliente '
                        wait 1 second
                        seek LAST in CL-NUEVO-F
               end-when
               read CL-NUEVO-F
    end-when
    when CLR   perform ACEP-COD-COND-VTA
;              move W-CON-VTA-COD to WW-CON-VTA-CAB
    end-when
    when KNEXT perform NEXT-SOCIAL.
    when KEND  exit.
    when ENTER perform ABRE-REF-PRODUCTO
               when OK move CL-N-LOCAL  to W-CLI-LOCAL
                       move CL-N-RUT    to W-CLI-RUT
                       move CL-N-DIG    to W-CLI-DIG
                       move CL-N-RAZON  to W-CLI-RAZON
                       move CL-N-DESPAC to W-CLI-DESPAC
                       move CL-N-VENDED to W-CLI-VENDED
                       move CL-N-VISITA to W-CLI-VISITA
                       move CL-N-DCT-BA to WW-DCT-CLIEN

                       move 0 to W-TOT-FACT
                       move 0 to W-TOTAL-IVA
                       move 0 to W-TOTAL-ILA
                       move 0 to AUX-LINEAS-MAX

                       move 0 to RE-WW-TOTAL-FAC ; Para Recalcular el IVA
                       move 0 to RE-WW-TOTAL-ILA

                       perform ABRE-UPD-DETALLES
                       perform ABRE-REF-PRODUCTO
                       seek FIRST in PRODUCTO-F
                       perform ACEP-COD-PRODUCTO

                       exit
               end-when
    end-when
loop.

NEXT-SOCIAL.
;-----------
    show W-NEXT-RAZON-sd
    action

    when KNEXT perform NEXT-CONDICION, exit.
    when KEND  exit.
loop.

NEXT-CONDICION.
;--------------
    show W-NEXT-CONDI-sd
    action

    when KEND exit.
loop.

ACEP-COD-COND-VTA.
;-----------------
    move ' ' to W-CON-VTA-DES
    show W-CON-VTA-DES-sd

    accept W-CON-VTA-COD-sd
    when OK if W-CON-VTA-COD = 0, loop.
            search FIRST for W-CON-VTA-COD in COND-VTA-F offset CON-CODIGO
            when ER beep
                    message 1 4 'Cond. No  Existe'
                    wait 1 second
                    message 1 4 '                '
            end-when
            when OK read COND-VTA-F
                    perform LOOP-COND-VENTA
                    exit
            end-when
    end-when
    when KEND move 0 to WW-CON-VTA-CAB, exit.
    when BAR  seek LAST in COND-VTA-F
              read COND-VTA-F
              perform LOOP-COND-VENTA
              exit
    end-when
    when FAR  seek FIRST in COND-VTA-F
              read COND-VTA-F
              perform LOOP-COND-VENTA
              exit
    end-when
loop.

LOOP-COND-VENTA.
;---------------

    move CON-CODIGO to W-CON-VTA-COD
    move CON-DESCRI to W-CON-VTA-DES

    show W-CON-VTA-COD-sd
    show W-CON-VTA-DES-sd

    FAST-ACTION
    when BAR   seek PRIOR in COND-VTA-F
               when EOF beep
                        seek FIRST in COND-VTA-F
               end-when
               read COND-VTA-F
    end-when
    when FAR   seek NEXT in COND-VTA-F
               when EOF beep
                        seek LAST in COND-VTA-F
               end-when
               read COND-VTA-F
    end-when
    when ENTER if W-CON-VTA-COD > W-CL-C-CONDIC
                  beep
                  message 1 4 'Codigo no Valido'
                  wait 1 second
                  clear 4
               else
                  move W-CON-VTA-COD to WW-CON-VTA-CAB
                  exit
               end-if
    end-when
    when KEND  move 0 to WW-CON-VTA-CAB, exit.
loop.

ACEP-COD-PRODUCTO.
;-----------------
    accept W-PRD-CODIGO-sd
    when OK   if W-PRD-CODIGO = 0, loop.
              search FIRST for W-PRD-CODIGO in PRODUCTO-F offset PRD-CODIGO
              when ER beep
                      message 1 4 '   No Existe    '
                      wait 1 second
                      message 1 4 '                '
              end-when
              when OK read PRODUCTO-F, perform LOOP-PRODUCTOS.
    end-when
    when KEND perform ABRE-REF-DETALLES
              when ER exit.
              when OK search FIRST for W-IDE-FACTUR in DETALLES-F offset DET-FACTURA ; por si tiene algun detalle loco
                      when ER exit.
                      when OK perform FIN-DE-DETALLE
                              when OK exit.
                      end-when
              end-when
    end-when
    when BAR  ;seek LAST in PRODUCTO-F
              read PRODUCTO-F
              perform LOOP-PRODUCTOS
    end-when
    when FAR  ;seek FIRST in PRODUCTO-F
              read PRODUCTO-F
              perform LOOP-PRODUCTOS
    end-when
loop.

LOOP-PRODUCTOS.
;--------------
    move PRD-CODIGO to W-PRD-CODIGO
    move PRD-DESCRI to W-PRD-DESCRI
    move PRD-UN-BAS to W-PRD-PRECIO

    move W-IDE-BODEGA to AUX-LL1-BOD
    move W-PRD-CODIGO to AUX-LL1-PRD

    search FIRST for AUX-LL1-EX-BODEGA in EX-BODEG-F offset EXI-LL1
    when ER move 0          to W-PRO-STOCK.
    when OK read EX-BODEG-F
            move EXI-SALDOO to W-PRO-STOCK
    end-when

    show W-PRD-CODIGO-sd
    show W-PRO-STOCK-sd
    show W-PRD-DESCRI-sd

    if PRD-OFERTA <> ' '
       message 1 4 '   EN OFERTA    '
    else
       message 1 4 '                '
    end-if

    FAST-ACTION

    when BAR   seek PRIOR in PRODUCTO-F
               when EOF beep
                        seek FIRST in PRODUCTO-F
               end-when
               read PRODUCTO-F
    end-when
    when FAR   seek NEXT in PRODUCTO-F
               when EOF beep
                        seek LAST in PRODUCTO-F
               end-when
               read PRODUCTO-F
    end-when
    when ENTER if AUX-LINEAS-MAX > 28
                  alarm
                  message 1 1 '****************'
                  message 1 2 'FACTURA COMPLETA'
                  message 1 3 'FACTURA COMPLETA'
                  message 1 4 '****************'
                  wait 1 second

               else

                  move W-IDE-FACTUR to W-DET-FACTURA
                  move W-PRD-CODIGO to W-DET-PRODUCT

                  search FIRST for W-DET-LL1 in DETALLES-F offset DET-LL1
                  when ER move PRD-DESCRI  to W-PRD-DESCRI-9
                          move PRD-UN-BAS  to W-PR-UN-BAS
                          move PRD-PR-VTA  to W-PR-UN-VTA
                          move PRD-DCT-PR  to WW-DCT-PRODU

                          perform CALC-UNIDAD-BASE  ; Unidad B sica
                          perform CALC-PRECIO-VENTA ; Unidad Venta

                          show W-COD-DES-PROD-sd
                          show W-PRECIO-UB-sd
                          show W-PRECIO-UV-sd

                          perform ACEP-CAN-PRODUCTO

                          add 1 to AUX-LINEAS-MAX

                  end-when
                  when OK beep
                          message 1 1 '****************'
                          message 1 2 '  Producto ya   '
                          message 1 3 '   Ingresado    '
                          message 1 4 '****************'
                          wait 1 second
                  end-when
               end-if
               exit
    end-when
    when KEND  exit.
loop.

CALC-UNIDAD-BASE.
;----------------
    move     W-PR-UN-VTA  to   TOTAL-A
    divide   W-PR-UN-BAS  into TOTAL-A
    move     TOTAL-A      to   TOTAL-B
    multiply TOTAL-B      by   WW-DCT-PRODU
    add      5000         to   TOTAL-b
    divide   10000        into TOTAL-B
    subtract TOTAL-B      from TOTAL-A

    move     TOTAL-A      to   TOTAL-C
    multiply TOTAL-C      by   WW-DCT-CLIEN
    add      5000         to   TOTAL-C
    divide   10000        into TOTAL-C
    subtract TOTAL-C      from TOTAL-A

    move     TOTAL-A      to   W-PRECIO-UB
exit.

CALC-PRECIO-VENTA.
;-----------------

    move     W-PR-UN-VTA  to   TOTAL-E
    move     TOTAL-E      to   TOTAL-F
    multiply TOTAL-F      by   WW-DCT-PRODU
    add      5000         to   TOTAL-F
    divide   10000        into TOTAL-F
    subtract TOTAL-F      from TOTAL-E

    move     TOTAL-E      to   TOTAL-G
    multiply TOTAL-G      by   WW-DCT-CLIEN
    add      5000         to   TOTAL-G
    divide   10000        into TOTAL-G
    subtract TOTAL-G      from TOTAL-E

    move     TOTAL-E      to   W-PRECIO-UV
exit.

ACEP-CAN-PRODUCTO.
;-----------------
    accept W-PRD-CANTID-sd
    when OK if W-PRD-CANTID = 0, loop.

            if W-PRD-CANTID > W-PRO-STOCK
               beep
               message 1 4 '  Insuficiente  '
               wait 1 second
               clear 4
            else

               move     W-PRECIO-UB  to W-TOT-PROD
               multiply W-TOT-PROD   by W-PRD-CANTID

               perform  VER-SI-ES-ILA

               perform MUESTRA-EL-TOT-PROD
               perform GRABA-EL-DETALLE

               exit
            end-if
    end-when
    when KEND exit.
loop.

VER-SI-ES-ILA.
;-------------
    add      W-TOT-PROD   to   RE-WW-TOTAL-FAC

    move     W-TOT-PROD   to   W-TOT-IVA
    multiply W-TOT-IVA    by   W-IDE-IVA
    add      5000         to   W-TOT-IVA
    divide   10000        into W-TOT-IVA
    add      W-TOT-IVA    to   W-TOT-FACT

    add      W-TOT-IVA    to   W-TOTAL-IVA

    if W-PRD-UNO > 2749 and W-PRD-UNO < 2752
       add      W-TOT-PROD   to   RE-WW-TOTAL-ILA

       move     W-TOT-PROD   to   W-TOT-ILA
       multiply W-TOT-ILA    by   W-IDE-ILA
       add      5000         to   W-TOT-ILA
       divide   10000        into W-TOT-ILA
       add      W-TOT-ILA    to   W-TOT-FACT

       add      W-TOT-ILA    to   W-TOTAL-ILA
    end-if

    add W-TOT-PROD to W-TOT-FACT

exit.

MUESTRA-EL-TOT-PROD.
;-------------------
    show TOTAL-DE-PRODUCTO-sd
    action

    when ENTER subtract W-PRD-CANTID from W-PRO-STOCK ; Rebajo Stock
               move     W-PRO-STOCK  to   EXI-SALDOO
               rewrite  EX-BODEG-R

               exit
    end-when
loop.

GRABA-EL-DETALLE.
;-----------------
    search BINARY for W-DET-LL1 in DETALLES-F offset DET-LL1
    when ER perform CALCULA-EL-DETALLE

            fill DETALLES-R with ' '

            move W-IDE-FACTUR     to DET-FACTURA
            move W-PRD-CODIGO     to DET-PRODUCT
            move W-PRD-CANTID     to DET-CAN-UNI
            move CALC-VAL-BRU-PRD to DET-VAL-BRU
            move WW-DCT-PRODU     to DET-DES-PRO
            move CALC-VAL-DCT-PRM to DET-VAL-PRO
            move WW-DCT-CLIEN     to DET-DES-BAS
            move CALC-VAL-DCT-BAS to DET-VAL-BAS
            move X'0D'            to DET-RET
            move X'0A'            to DET-FLI

            insert DETALLES-R

    end-when
exit.

CALCULA-EL-DETALLE.
;------------------

  ; Inicio  Calculo Valor Bruto del Producto del Detalle

    move     W-PR-UN-VTA      to   CALC-VAL-BRU-PRD
    divide   W-PR-UN-BAS      into CALC-VAL-BRU-PRD

  ; Termino Calculo Valor Bruto del Producto del Detalle



  ; Inicio  Calculo Valor Descuento Promocional

    move     WW-DCT-PRODU     to   CALC-VAL-DCT-PRM
    multiply CALC-VAL-DCT-PRM by   CALC-VAL-BRU-PRD
    add      5000             to   CALC-VAL-DCT-PRM
    divide   10000            into CALC-VAL-DCT-PRM
    subtract CALC-VAL-DCT-PRM from CALC-VAL-BRU-PRD

  ; Termino Calculo Valor Descuento Promocional


  ; Inicio  Calculo Valor Descuento Base

    move     WW-DCT-CLIEN     to   CALC-VAL-DCT-BAS
    multiply CALC-VAL-DCT-BAS by   CALC-VAL-BRU-PRD
    add      5000             to   CALC-VAL-DCT-BAS
    divide   10000            into CALC-VAL-DCT-BAS
    subtract CALC-VAL-DCT-BAS from CALC-VAL-BRU-PRD

  ; Termino Calculo Valor Descuento Base


    multiply CALC-VAL-BRU-PRD by   W-PRD-CANTID

exit.

FIN-DE-DETALLE.
;--------------
    beep
    message 1 1 'Finaliza Detalle'
    message 1 2 '****************'
    message 1 3 '   [SI]  [NO]   '
    message 1 4 '****************'
    action
    when ENTER  perform TOTAL-TOTAL-VENTA, exit.
    when NO     exit.
loop.

TOTAL-TOTAL-VENTA.
;-----------------
    beep
    message 1 1 ' Finaliza Venta '
    message 1 2 '****************'
    message 1 3 '   [SI]  [NO]   '
    message 1 4 '****************'
    action
    when ENTER  perform ABRE-UPD-CABECERA

                if MODO-OPERACION = 1
                   perform GRABA-LA-CEBECERA
                else

                   perform ABRE-REF-CABECERA
                   search FIRST for W-IDE-FACTUR in CABECERA-F offset CAB-LL1
                   when OK read CABECERA-F
                           perform ACTUALIZAR-CABECER
                   end-when
                end-if

                perform SUMA-1-A-LA-FACTURA
                move OK to LAST-KEY
                exit
    end-when
    when NO     move ER to LAST-KEY, exit.
loop.

GRABA-LA-CEBECERA.
;-----------------
    move W-IDE-FACTUR to W-CAB-FACTURA
    search BINARY for W-CAB-LL1 in CABECERA-F offset CAB-LL1
    when ER fill CABECERA-R with ' '

            move W-IDE-FACTUR      to CAB-FACTURA
            move W-CLI-LOCAL       to CAB-LOC-CLI
            move W-CLI-RUT         to CAB-RUT-RUT
            move W-CLI-DIG         to CAB-RUT-DIG

            perform LEE-FECHA-HORA

            move DIA-WS            to AUX-WW-DIA
            move MES-WS            to AUX-WW-MES
            move ANO-WS            to AUX-WW-ANO

            move DIA-WS            to WW-DIA
            move MES-WS            to WW-MES
            move ANO-WS            to WW-ANO

            move HORA-WS           to AUX-WW-HH
            move MINU-WS           to AUX-WW-MM

            move AUX-FECHA-FILE    to CAB-FEC-FAC
            move AUX-HORA-FILE     to CAB-HOR-FAC

            perform CALCULA-DIA-VENCIMIENTO

            move WW-DIA            to AUX-WW-DIA
            move WW-MES            to AUX-WW-MES
            move WW-ANO            to AUX-WW-ANO
            move AUX-FECHA-FILE    to CAB-FEC-VEN

            move W-CLI-VENDED      to CAB-COD-VEN
            move W-CL-C-CONDIC     to CAB-CON-VEN

            perform RECALCULA-LOS-TOTALES

            move RE-WW-TOTAL-IVA   to CAB-VAL-IVA
            move RE-WW-TOTAL-ILA   to CAB-VAL-ILA
            move RE-WW-TOTAL-FAC   to CAB-TOT-FAC
            move ' '               to CAB-FAC-IMP
            move ' '               to CAB-FAC-NUL
            move ' '               to CAB-TIP-CLI
            move X'0D'             to CAB-RET
            move X'0A'             to CAB-FLI

            insert CABECERA-R
    end-when
exit.

RECALCULA-LOS-TOTALES.
;---------------------
    move     RE-WW-TOTAL-FAC to   RE-WW-TOTAL-IVA
    multiply RE-WW-TOTAL-IVA by   W-IDE-IVA
    add      5000            to   RE-WW-TOTAL-IVA
    divide   10000           into RE-WW-TOTAL-IVA
    add      RE-WW-TOTAL-IVA to   RE-WW-TOTAL-FAC

    multiply RE-WW-TOTAL-ILA by   W-IDE-ILA
    add      5000            to   RE-WW-TOTAL-ILA
    divide   10000           into RE-WW-TOTAL-ILA
    add      RE-WW-TOTAL-ILA to   RE-WW-TOTAL-FAC

exit.

CALCULA-DIA-VENCIMIENTO.
;-----------------------
    move 1 to X
    perform LLENA-ARREGLO
    perform SUMA-UN-DIA
exit.

LLENA-ARREGLO.
;-------------
    move 31 to ARR-DIA(1)
    move 28 to ARR-DIA(2)
    move 31 to ARR-DIA(3)
    move 30 to ARR-DIA(4)
    move 31 to ARR-DIA(5)
    move 30 to ARR-DIA(6)
    move 31 to ARR-DIA(7)
    move 31 to ARR-DIA(8)
    move 30 to ARR-DIA(9)
    move 31 to ARR-DIA(10)
    move 30 to ARR-DIA(11)
    move 31 to ARR-DIA(12)
exit.

SUMA-UN-DIA.
;-----------
    add 1 to WW-DIA

    move WW-MES to NUM-MES
    if WW-DIA > ARR-DIA(NUM-MES)

       move 1 to WW-DIA
       add  1 to WW-MES

       if WW-MES > 12
          move 1 to WW-MES
          add  1 to WW-ANO
       end-if
    end-if

    add 1 to X
    if X > W-NEXT-VENCI, exit.
loop.

SUMA-1-A-LA-FACTURA.
;-------------------
    add 1             to W-IDE-FACTUR
    move W-IDE-FACTUR to IDE-FAC-EM
    rewrite IDENTIFI-R
exit.

VENT-IMPRIME.
;------------
    move CAB-FACTURA to IMP-CAB-FAC
    move CAB-LOC-CLI to IMP-CAB-LOC
    move CAB-RUT-CLI to IMP-CAB-RUT
    move CAB-RUT-DIG to IMP-CAB-DIG
    move CAB-TOT-FAC to IMP-CAB-TOT
    move CAB-FEC-FAC to AUX-FECHA-FILE

    perform ACCESAR-CLIENTE

    show IMP-CAB-FACTURA-sd
    FAST-ACTION

    when BAR    perform LOOP-ARRIBA.
    when FAR    perform LOOP-ABAJO.

    when ENTER  if CAB-FAC-IMP <> ' '
                   beep
                   message 1 4 '    Impresa     '
                   wait 1 second
                else
                   if CAB-FAC-NUL <> ' '
                      beep
                      message 1 4 '    Anulada     '
                      wait 1 second
                   else
                      move CAB-FACTURA to W-IDE-FACTUR
                      perform VERIFICA-NRO-FACTU
                   end-if
                end-if
    end-when
    when KEND   exit.
loop.

LOOP-ARRIBA.
;-----------
    seek PRIOR in CABECERA-F
    when EOF beep
             message 1 4 'Primera  Factura'
             wait 1 second
             seek FIRST in CABECERA-F
             perform FIRST-PRIMERO
             exit
    end-when
    read CABECERA-F
    if CAB-FAC-IMP = ' '
       exit
    end-if
loop.

FIRST-PRIMERO.
;-------------
    read CABECERA-F
    if CAB-FAC-IMP = ' '
       exit
    end-if
    seek NEXT in CABECERA-F
    when EOF beep
             message 1 4 ' Ultima  Factura'
             wait 1 second
             exit
    end-when
loop.

LOOP-ABAJO.
;----------
    seek NEXT in CABECERA-F
    when EOF beep
             message 1 4 ' Ultima Factura '
             wait 1 second
             seek LAST in CABECERA-F
             perform LAST-ULTIMO
             exit
    end-when
    read CABECERA-F
    if CAB-FAC-IMP = ' '
       exit
    end-if
loop.

LAST-ULTIMO.
;-----------
    read CABECERA-F
    if CAB-FAC-IMP = ' '
       exit
    end-if
    seek PRIOR in CABECERA-F
    when EOF beep
             message 1 1 'Primera Factura '
             wait 1 second
             exit
    end-when
loop.



VERIFICA-NRO-FACTU.
;------------------
    clear
    show W-IDE-FACTUR-sd
    show W-NUE-NRO-FACT-sd

    ACTION

    when ENTER perform IMPRIME-LA-FACTURA, exit.
    when KEND  exit.
    when CLR   perform ACEP-NRO-FAC-IMPR.
loop.

ACEP-NRO-FAC-IMPR.
;-----------------
    accept W-NUE-NRO-FACT-sd
    when OK if W-IDE-FACTUR = 0, loop.
            perform SI-YA-FUE-EMITIDA
            when ER beep
                    message 1 2 ' Esta Seguro ?  '
                    message 1 3 '****************'
                    message 1 4 '   [SI]  [NO]   '

                    ACTION

                    when NO    exit.
                    when ENTER move CAB-FACTURA  to AUX-RES-NRO-FAC
                               move W-IDE-FACTUR to CAB-FACTURA
                               rewrite CABECERA-R

                               perform CAMB-NRO-FAC-EN-DETALLE
                               exit
                    end-when
            end-when
    end-when
    when KEND exit.
loop.

CAMB-NRO-FAC-EN-DETALLE.
;-----------------------
    search FIRST for AUX-RES-NRO-FAC in DETALLES-F offset DET-FACTURA
    when ER exit.
    when OK read DETALLES-F
            move W-IDE-FACTUR to DET-FACTURA
            rewrite DETALLES-R
    end-when
loop.

IMPRIME-LA-FACTURA.
;------------------
    beep
    message  1 1 'IMPRIME  FACTURA'
    message  1 2 '****************'
    message  1 3 '   [SI]  [NO]   '
    message  1 4 '****************'
    ACTION

    when NO    exit.
    when ENTER beep
               message 1 1 'Revise  Conexion'
               message 1 2 'PTC -- IMPRESORA'
               message 1 3 '****************'
               message 1 4 '   [SI]  [NO]   '
               ACTION

               when NO    exit.
               when ENTER CONNECT IMPRESORA-ZFP-80
                          when ER alarm
                                  message  1 1 '*****************'
                                  message  1 2 '    Error en     '
                                  message  1 3 '    Conexion     '
                                  message  1 4 '*****************'
                                  wait 3 second
                                  loop
                          end-when
                          when OK message  1 1 '****************'
                                  message  1 2 '   Inicio de    '
                                  message  1 3 '  Impresion...  '
                                  message  1 4 '****************'

                                  perform ABRE-REF-CONDICIO
                                  perform IMPRIME-LA-CABECERA

                                  when OK seek FIRST in DETALLES-F
                                          when OK move 0 to NUM-LIN-DETALLE
                                                  move 0 to PRN-TOTAL-NETO
                                                  move 0 to PRN-TOTAL-ILA
                                                  move 0 to PRN-TOTAL-IVA

                                                  perform IMPRIME-EL-DETALLE

                                                  DISCONNECT

                                                  beep
                                                  message  1 1 '****************'
                                                  message  1 2 '   Termino de   '
                                                  message  1 3 '  Impresion...  '
                                                  message  1 4 '****************'
                                                  wait 1 second

                                          end-when

                                          perform IMPRESION-OK-SI-NO

                                  end-when
                          end-when
                          exit
               end-when
    end-when
loop.

IMPRIME-LA-CABECERA.
;-------------------
    move CAB-FACTURA to PRN-CAB-NUM
    move CAB-FEC-FAC to PRN-CAB-FEC
    move CAB-FEC-VEN to PRN-CAB-VEN
    move CAB-HOR-FAC to PRN-CAB-HOR
    move CAB-RUT-CLI to PRN-CAB-RUT
    move CAB-COD-VEN to PRN-CAB-VED
    move CAB-LOC-CLI to PRN-CAB-LOC

    move IMP-CAB-RAZ to PRN-CAB-RAZ
    move IMP-CAB-DES to PRN-CAB-DES
    move IMP-CAB-CIU to PRN-CAB-CIU

    move CAB-VAL-IVA to PRN-CAB-IVA
    move CAB-VAL-ILA to PRN-CAB-ILA
    move CAB-TOT-FAC to PRN-CAB-FAC

    search FIRST for CAB-CON-VEN in COND-VTA-F offset CON-CODIGO
    when ER move 'SIN CONDICION' to PRN-DET-COND.
    when OK read COND-VTA-F
            move CON-DESCRI      to PRN-DET-COND
    end-when

    perform LEE-FECHA-HORA
    move HORA-WS           to AUX-WW-HH
    move MINU-WS           to AUX-WW-MM

    build CABECERA-RP print

exit.

IMPRIME-EL-DETALLE.
;------------------
    read DETALLES-F
    if DET-FACTURA = CAB-FACTURA

       add 1 to NUM-LIN-DETALLE

       move     DET-PRODUCT to   PRN-DET-PRD
       move     DET-CAN-UNI to   PRN-DET-CAN

       move     DET-VAL-BRU to   PRN-DET-UNI
       divide   PRN-DET-CAN into PRN-DET-UNI

       move     DET-VAL-BRU to   PRN-DET-NET

       add      PRN-DET-NET to   PRN-TOTAL-NETO

       search FIRST for PRN-DET-PRD in PRODUCTO-F offset PRD-CODIGO
       when ER move ' '        to PRN-DET-DES.
       when OK read PRODUCTO-F
               move PRD-DESCRI to PRN-DET-DES
       end-when

       build DETALLE-RP print

    end-if

    seek NEXT in DETALLES-F

    when EOF move     29              to   NUM-LIN-27
             subtract NUM-LIN-DETALLE from NUM-LIN-27
             move     0               to   XXX

             perform LOOP-ESPACIOS

             build HORA-RP      print

             ; 08 de Mayo de 1996
             ; INICIO Recalculo el IVA (en base al PRN-TOTAL-NETO)

             move     PRN-TOTAL-NETO  to   PRN-CAB-IVA
             multiply PRN-CAB-IVA     by   W-IDE-IVA
             add      5000            to   PRN-CAB-IVA
             divide   10000           into PRN-CAB-IVA

             move     PRN-CAB-IVA     to   CAB-VAL-IVA
             rewrite  CABECERA-R

             ; FIN    Recalculo el IVA (en base al PRN-TOTAL-NETO)

             build TOTAL-RP     print
             build SALTO-PAG-RP print

             exit
    end-when
loop.

LOOP-ESPACIOS.
;-------------
    build ESPACIOS-RP print

    add 1 to XXX
    if XXX > NUM-LIN-27, exit.

loop.

IMPRESION-OK-SI-NO.
;------------------
    move 'X' to CAB-FAC-IMP
    rewrite CABECERA-R
exit.

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³                INICIO OPCION "CLIENTES"  (Men£ Principal)                ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

PRN-CLIENTES.
;------------
    clear
    perform ABRE-REF-CLI-CART
    perform ABRE-REF-CLI-RUTA
    perform ABRE-REF-CONDICIO
    perform ABRE-UPD-CLI-NUEV
    move  1  to INDICE
    move  1  to MIN-INDICE
    move  9  to MAX-INDICE

    move  0  to W-CL-N-LOCAL
    move  0  to W-CL-N-RUT
    move ' ' to W-CL-N-DIG
    move ' ' to W-CL-N-RAZON
    move ' ' to W-CL-N-DESPAC
    move  0  to W-CL-N-CONDIC
    move  0  to W-CL-N-DCT-BA
    move  0  to W-CL-N-VENDED
    move ' ' to W-CL-N-VISITA
    move ' ' to W-CL-N-CIUDAD

    perform CLIE-NUEVO
exit.

CLIE-NUEVO.
;----------
   if INDICE =  1 perform SH-AC-RUT.
   if INDICE =  2 perform SH-AC-LOCAL.
   if INDICE =  3 perform SH-AC-RAZON.
   if INDICE =  4 perform SH-AC-DESPACHO.
   if INDICE =  5 perform SH-AC-CONDICIO.
   if INDICE =  6 perform SH-AC-DCT-BASE.
   if INDICE =  7 perform SH-AC-CIUDAD.
   when OK if INDICE = 7 perform GRABA-CLI-NUEVO, exit.
           perform AVA-INDICE
   end-when
   when BAR if INDICE = 1 loop.
            perform RET-INDICE
   end-when
   when KEND exit.
loop.


SH-AC-RUT.
;---------
    if W-CL-N-RUT = 0
       perform AC-RUT
    else
       perform SH-RUT
    end-if
exit.

AC-RUT.
;------
    accept W-CL-N-RUT-sd
    when OK if W-CL-N-RUT = 0, loop.
            perform AC-DIGITO
            exit
    end-when
    when KEND exit.
    when BAR  exit.
loop.

AC-DIGITO.
;---------
    accept W-CL-N-DIG-sd
    when OK if W-CL-N-DIG = ' ', loop.

            move W-CL-N-RUT to RUT-1
            move 2          to WS-CON
            move 8          to WS-TBL
            move 0          to WS-SUMA
            move 0          to RESTO-AUX1
            perform VALIDA-EL-DIGITO

            if WS-DIG = W-CL-N-DIG
               move OK to LAST-KEY
               exit
            else
               beep
               message 1 4 'Digito  Invalido'
               wait 1 second
               message 1 4 '                '
            end-if
    end-when
    when KEND exit.
loop.

SH-RUT.
;------
    show W-CL-N-RUT-sd
    show W-CL-N-DIG-sd
    ACTION

    when ENTER move OK to LAST-KEY, exit.
    when KEND  exit.
    when BAR   exit.
    when CLR   perform AC-RUT, exit.
loop.

VALIDA-EL-DIGITO.
;----------------
    if ws-tbl < 1
       move ws-suma to ws-resto1
       perform COMPARA
       exit
     end-if
     move     WS-NUM(WS-TBL) to WS-NUM-AUX
     multiply WS-NUM-AUX     by WS-CON
     add      WS-NUM-AUX     to WS-SUMA
     if WS-CON = 7
          move 1 to WS-CON
     end-if

     subtract 1 from WS-TBL
     add      1 to   WS-CON
loop.

COMPARA.
;-------
    divide   11 into WS-RESTO1 remainder RESTO-AUX1
    subtract 11 from RESTO-AUX1
    if RESTO-AUX1 = 10
       move 'K' to WS-DIG
       exit
    end-if
    if RESTO-AUX1 = 11
       move 0 to WS-DIG
       exit
    end-if
    move RESTO-AUX1 to RESTO-AUX
    move RESTO-AUX  to WS-DIG
exit.

SH-AC-LOCAL.
;-----------
    if W-CL-N-LOCAL = 0
       perform AC-LOCAL
    else
       perform SH-LOCAL
    end-if
exit.

AC-LOCAL.
;--------

    show W-CL-N-RUT-sd
    show W-CL-N-DIG-sd

    accept W-CL-N-LOCAL-sd
    when OK search FIRST for W-CL-N-LL1 in CL-RUTA-F offset CL-R-LL1
            when ER search FIRST for W-CL-N-LL1 in CL-CARTE-F offset CL-C-LL1
                    when ER search FIRST for W-CL-N-LL1 in CL-NUEVO-F offset CL-N-LL1
                            when OK move 0 to GRABA-CLI-NUEV
                                    read CL-NUEVO-F
                                    move CL-N-RAZON  to W-CL-N-RAZON
                                    move CL-N-DESPAC to W-CL-N-DESPAC
                                    move CL-N-CONDIC to W-CL-N-CONDIC
                                    move CL-N-DCT-BA to W-CL-N-DCT-BA
                                    move CL-N-VENDED to W-CL-N-VENDED
                                    move CL-N-VISITA to W-CL-N-VISITA
                                    move CL-N-CIUDAD to W-CL-N-CIUDAD
                            end-when
                            when ER move OK to LAST-KEY, move 1 to GRABA-CLI-NUEV.
                    end-when
                    when OK  beep
                             message 1 1 '****************'
                             message 1 2 '   Cliente en   '
                             message 1 3 '    CARTERA     '
                             message 1 4 '****************'
                             wait 1 second
                             clear
                             move ER to LAST-KEY
                    end-when
            end-when
            when OK beep
                    message 1 1 '****************'
                    message 1 2 '   Cliente en   '
                    message 1 3 '      RUTA      '
                    message 1 4 '****************'
                    wait 1 second
                    clear
                    move ER to LAST-KEY
            end-when

            exit
    end-when
    when KEND exit.
loop.

SH-LOCAL.
;--------
    show W-CL-N-LOCAL-sd
    action

    when ENTER move OK to LAST-KEY, exit.
    when KEND  exit.
    when BAR   exit.
    when CLR   perform AC-LOCAL, exit.
loop.

SH-AC-RAZON.
;-----------
    if W-CL-N-RAZON = ' '
       perform AC-RAZON
    else
       perform SH-RAZON
    end-if
exit.

AC-RAZON.
;--------
    accept W-CL-N-RAZON-sd
    when OK if W-CL-N-RAZON = ' ', loop.
            exit
    end-when
    when KEND exit.
    when BAR  clear, exit.
loop.

SH-RAZON.
;--------
    show W-CL-N-RAZON-sd
    action

    when ENTER move OK to LAST-KEY, exit.
    when KEND  exit.
    when BAR   clear, exit.
    when CLR   perform AC-RAZON, exit.
loop.

SH-AC-DESPACHO.
;--------------
    if W-CL-N-DESPAC = ' '
       perform AC-DESPACHO
    else
       perform SH-DESPACHO
    end-if
exit.

AC-DESPACHO.
;-----------
    accept W-CL-N-DESPAC-sd
    when OK if W-CL-N-DESPAC = ' ', loop.
            clear
            exit
    end-when
    when KEND exit.
    when BAR  exit.
loop.

SH-DESPACHO.
;-----------
    show W-CL-N-DESPAC-sd
    action

    when ENTER move OK to LAST-KEY, clear, exit.
    when KEND  exit.
    when BAR   exit.
    when CLR   perform AC-DESPACHO, exit.
loop.

SH-AC-CONDICIO.
;--------------
    if W-CL-N-CONDIC = 0
       perform AC-CONDICIO
    else
       perform SH-CONDICIO
    end-if
exit.

AC-CONDICIO.
;-----------
    accept W-CL-N-CONDIC-sd
    when OK if W-CL-N-CONDIC = 0, loop.
            search FIRST for W-CL-N-CONDIC in COND-VTA-F offset CON-CODIGO
            when ER beep
                    message 1 4 '   No Existe    '
                    wait 1 second
                    message 1 4 '                '
            end-when
            when OK exit.
    end-when
    when KEND exit.
    when BAR  exit.
loop.

SH-CONDICIO.
;-----------
    show W-CL-N-CONDIC-sd
    action

    when ENTER move OK to LAST-KEY, exit.
    when KEND  exit.
    when BAR   exit.
    when CLR   perform AC-CONDICIO, exit.
loop.

SH-AC-DCT-BASE.
;--------------
    if W-CL-N-DCT-BA = 0
       perform AC-DCT-BASE
    else
       perform SH-DCT-BASE
    end-if
exit.

AC-DCT-BASE.
;-----------
    accept W-CL-N-DCT-BA-sd
    when OK   exit.
    when KEND exit.
    when BAR  exit.
loop.

SH-DCT-BASE.
;-----------
    show W-CL-N-DCT-BA-sd
    action

    when ENTER move OK to LAST-KEY, exit.
    when KEND  exit.
    when BAR   exit.
    when CLR   perform AC-DCT-BASE, exit.
loop.

SH-AC-CIUDAD.
;------------
    if W-CL-N-CIUDAD = ' '
       perform AC-CIUDAD
    else
       perform SH-CIUDAD
    end-if
exit.

AC-CIUDAD.
;---------
    accept W-CL-N-CIUDAD-sd

    when OK exit.
    when KEND exit.
    when BAR  exit.
loop.

SH-CIUDAD.
;---------
    show W-CL-N-CIUDAD-sd
    action

    when ENTER move OK to LAST-KEY, exit.
    when KEND  exit.
    when BAR   exit.
    when CLR   perform AC-CIUDAD, exit.
loop.

GRABA-CLI-NUEVO.
;---------------
    if GRABA-CLI-NUEV = 1
       fill CL-NUEVO-R    with ' '
       move W-CL-N-LOCAL  to CL-N-LOCAL
       move W-CL-N-RUT    to CL-N-RUT
       move W-CL-N-DIG    to CL-N-DIG
       move W-CL-N-RAZON  to CL-N-RAZON
       move W-CL-N-DESPAC to CL-N-DESPAC
       move W-CL-N-CONDIC to CL-N-CONDIC
       move W-CL-N-DCT-BA to CL-N-DCT-BA
       move W-IDE-VENDED  to CL-N-VENDED
       move W-IDE-VISITA  to CL-N-VISITA
       move W-CL-N-CIUDAD to CL-N-CIUDAD
       move X'0D'         to CL-N-RET
       move X'0A'         to CL-N-FLI

       write CL-NUEVO-R
    else

       move W-CL-N-RAZON  to CL-N-RAZON
       move W-CL-N-DESPAC to CL-N-DESPAC
       move W-CL-N-CONDIC to CL-N-CONDIC
       move W-CL-N-DCT-BA to CL-N-DCT-BA

       rewrite CL-NUEVO-R
    end-if
exit.

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³              TERMINO OPCION "CLIENTES"  (Men£ Principal)                 ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

PRN-CANJEAR.
;-----------
    clear
    accept CANJ-COD-PRD-sd
    when OK if CANJ-COD-PRD <> 0
               search FIRST for CANJ-COD-PRD in PRODUCTO-F offset PRD-CODIGO
               when OK read PRODUCTO-F
                       perform LOOP-PRD-CANJEAR
               end-when
            else
               seek FIRST in PRODUCTO-F
               when OK read PRODUCTO-F
                       perform LOOP-PRD-CANJEAR
               end-when
            end-if
    end-when
    when BAR  seek FIRST in PRODUCTO-F, read PRODUCTO-F, perform LOOP-PRD-CANJEAR.
    when FAR  seek LAST  in PRODUCTO-F, read PRODUCTO-F, perform LOOP-PRD-CANJEAR.
    when KEND exit.
loop.

LOOP-PRD-CANJEAR.
;----------------
    move PRD-CODIGO to CANJ-COD-PRD
    move PRD-DESCRI to CANJ-DES-PRD

    move W-IDE-BODEGA to AUX-LL1-BOD
    move CANJ-COD-PRD to AUX-LL1-PRD

    search FIRST for AUX-LL1-EX-BODEGA in EX-BODEG-F offset EXI-LL1
    when ER move 0          to W-PRO-STOCK
            move 0          to CANJ-ULT-CANJE
            move 0          to CANJ-ACU-CANJE
    end-when
    when OK read EX-BODEG-F
            move EXI-SALDOO to W-PRO-STOCK
            move EXI-UL-CAN to CANJ-ULT-CANJE
            move EXI-CANJEA to CANJ-ACU-CANJE
    end-when

    show CANJ-COD-PRD-sd
    show CANJ-DES-PRD-sd
    show CANJ-ACU-CANJE-sd

             ACTION

    when BAR    seek PRIOR in PRODUCTO-F
                when EOF beep
                         message 1 4 'Primer  Producto'
                         wait 1 second
                         clear 4
                         seek FIRST in PRODUCTO-F
                end-when
                read PRODUCTO-F
    end-when
    when FAR    seek NEXT in PRODUCTO-F
                when EOF beep
                         message 1 4 'Ultimo  Producto'
                         wait 1 second
                         clear 4
                         seek LAST in PRODUCTO-F
                end-when
                read PRODUCTO-F
    end-when
    when KEND   exit.
    when ENTER  show CANJ-ACU-2-CANJ-sd
                perform ACEP-CAN-CANJE
                clear
    end-when
loop.


ACEP-CAN-CANJE.
;--------------
    accept CANJ-CAN-CANJE-sd
    when OK if CANJ-CAN-CANJE = 0, loop.
            if CANJ-CAN-CANJE > W-PRO-STOCK
               beep
               message 1 4 '  Insuficiente  '
               wait 1 second
               clear 4
            else
               perform CALCULA-EL-CANJE

               rewrite  EX-BODEG-R
               exit

            end-if
    end-when
    when KEND exit.
loop.

CALCULA-EL-CANJE.
;----------------
    move     CANJ-CAN-CANJE to   EXI-UL-CAN
    add      CANJ-CAN-CANJE to   EXI-CANJEA
    subtract CANJ-CAN-CANJE from EXI-SALDOO
exit.

PRN-ABASTECIMIENTO.
;------------------
    read PRODUCTO-F

    move W-IDE-BODEGA to AUX-LL1-BOD
    move PRD-CODIGO   to AUX-LL1-PRD

    search FIRST for AUX-LL1-EX-BODEGA in EX-BODEG-F offset EXI-LL1
    when ER move 0          to W-EXI-INICIA
            move 0          to W-EXI-SALDOO
            move 0          to W-EXI-CANJEA
    end-when
    when OK read EX-BODEG-F
            move EXI-INICIA to W-EXI-INICIA
            move EXI-SALDOO to W-EXI-SALDOO
            move EXI-CANJEA to W-EXI-CANJEA
    end-when

;   if W-EXI-INICIA <> W-EXI-SALDOO   ; No se ha hecho ning£n Movimiento
    if W-EXI-INICIA > 0
       perform GENERA-LA-SOLICITUD
    end-if

    seek NEXT in PRODUCTO-F
    when EOF exit.

loop.

GENERA-LA-SOLICITUD.
;-------------------
    fill SO-ABAST-R with ' '

    move     AUX-LL1-BOD    to   SOL-BODEGA
    move     AUX-LL1-PRD    to   SOL-PRODUC
    move     W-EXI-INICIA   to   SOL-INICIA
    move     W-EXI-SALDOO   to   SOL-FINALL
    move     W-EXI-INICIA   to   SOL-UN-BAS
    subtract W-EXI-SALDOO   from SOL-UN-BAS
    move     W-EXI-CANJEA   to   SOL-TOTALL
    move     X'0D'          to   SOL-RET
    move     X'0A'          to   SOL-FLI

    write SO-ABAST-R
exit.

GENERA-DE-NUEVO.
;---------------
    message 1 1 '  Genera NUEVA  '
    message 1 2 '   Solicitud    '
    message 1 3 '****************'
    message 1 4 '   [SI]  [NO]   '

    ACTION

    when NO    exit.
    when ENTER perform ABRE-UPD-SO-ABAST
               ERASE SO-ABAST-F

               perform ABRE-UPD-SO-ABAST
               perform ABRE-REF-EXISTENC
               perform ABRE-REF-PRODUCTO

               when OK seek FIRST in PRODUCTO-F
                       when OK message 1 1 '****************'
                               message 1 2 ' Generacion en  '
                               message 1 3 '   Proceso...   '
                               message 1 4 '****************'

                               perform PRN-ABASTECIMIENTO

                               beep
                               message 1 1 '****************'
                               message 1 2 '   Solicitud    '
                               message 1 3 '    Generada    '
                               message 1 4 '****************'
                               wait 1 second
                       end-when
               end-when
    end-when
loop.

ACEP-PASSWORD.
;-------------
    accept WW-PASSWORD-SD
    when OK if WW-PASSWORD = ' ', loop.
            if WW-PASSWORD = W-IDE-PASSWO or WW-PASSWORD = 'TELX'

               move 1 to INDICE
               move 1 to IND-PANTALLA
               perform PRN-SUPERVISOR

               exit
            else
               beep
            end-if
    end-when
    when KEND exit.
loop.

PRN-SUPERVISOR.
;--------------
    move 1 to MIN-INDICE
    move 3 to MAX-INDICE
    if IND-PANTALLA > 0
       message 1 1 ' * Supervisor * '
       message 1 2 '  Transmision   '
       message 1 3 '  Identificacion'
       message 1 4 '  Fecha y Hora  '
       move 0 to IND-PANTALLA
    end-if
    examine INDICE
    when 1 message 1 2 '>'
           message 1 3 ' '
           message 1 4 ' '.

    when 2 message 1 2 ' '
           message 1 3 '>'
           message 1 4 ' '.

    when 3 message 1 2 ' '
           message 1 3 ' '
           message 1 4 '>'.

    FAST-ACTION

    when FAR perform AVA-INDICE.
    when UAR perform AVA-INDICE.
    when BAR perform RET-INDICE.
    when DAR perform RET-INDICE.
    when KEND  exit.
    when ENTER move indice to IND-MENU4
               perform OPC-SUPERVISOR
               move IND-MENU4 to INDICE
               move 1 to IND-PANTALLA.
loop.

OPC-SUPERVISOR.
;--------------
    examine IND-MENU4
       when 1 move 1 to INDICE
              move 1 to IND-PANTALLA
              perform SUPE-TRANSMISION
       end-when
       when 2 perform SUPE-IDENTIFICAC.
       when 3 perform SUPE-FECHA-HORA.
exit.

SUPE-TRANSMISION.
;----------------
    move 1 to MIN-INDICE
    move 2 to MAX-INDICE
    if IND-PANTALLA > 0
       message 1 1 '  Transmision   '
       message 1 2 '                '
       message 1 3 '  Recibir Datos '
       message 1 4 '  Enviar  Datos '
       move 0 to IND-PANTALLA
    end-if

    examine INDICE

    when 1 message 1 2 ' '
           message 1 3 '>'
           message 1 4 ' '.

    when 2 message 1 2 ' '
           message 1 3 ' '
           message 1 4 '>'.

    FAST-ACTION

    when FAR perform AVA-INDICE.
    when UAR perform AVA-INDICE.
    when BAR perform RET-INDICE.
    when DAR perform RET-INDICE.
    when KEND  exit.
    when ENTER move INDICE to IND-MENU5
               perform OPC-TRANSMISION
               move IND-MENU5 to INDICE
               move 1 to IND-PANTALLA.
loop.

OPC-TRANSMISION.
;---------------
    examine IND-MENU5
       when 1 perform RECIBIR-DATOS.
       when 2 perform ENVIAR-DATOS.
exit.

RECIBIR-DATOS.
;-------------
   perform CONECTA-HOST
   if LAST-KEY <> OK exit.

    move 'IDENTIFI'  to ARCH-A-RECIBIR
    move '.'         to R-PUNTO
    move 'D'         to S-EXTENSION
    perform REC-ARCHIVO

   if LAST-KEY = OK
      move 'CL-CARTE'  to ARCH-A-RECIBIR
      move '.'         to R-PUNTO
      move 'D'         to S-EXTENSION
      perform REC-ARCHIVO

      if LAST-KEY = OK
         move 'CL-RUTA'   to ARCH-A-RECIBIR
         move '.'         to R-PUNTO
         move 'D'         to S-EXTENSION
         perform REC-ARCHIVO

         if LAST-KEY = OK
            move 'COND-VTA'  to ARCH-A-RECIBIR
            move '.'         to R-PUNTO
            move 'D'         to S-EXTENSION
            perform REC-ARCHIVO

            if LAST-KEY = OK
               move 'PRODUCTO'  to ARCH-A-RECIBIR
               move '.'         to R-PUNTO
               move 'D'         to S-EXTENSION
               perform REC-ARCHIVO

               if LAST-KEY = OK
                  move 'EX-BODEG'  to ARCH-A-RECIBIR
                  move '.'         to R-PUNTO
                  move 'D'         to S-EXTENSION
                  perform REC-ARCHIVO

               end-if
            end-if
         end-if
      end-if
   end-if

   clear
   if LAST-KEY = ER
      fill CMD-REC-WS with ' '
      move '81' to CMD                ;Status Malo!!
      perform ENVIA-COMANDO-HOST
   end-if

   perform DESCONECTA-HOST
exit.

ENVIAR-DATOS.
;------------
   perform CONECTA-HOST
   if LAST-KEY <> OK exit.

       perform ABRE-REF-CABECERA
       move AUX-FILE-NAME  to FILE-NAME
       move 'D'            to FILE-TYPE
       perform ENVIA-ARCHIVO

       if LAST-KEY = OK
          perform ABRE-REF-DETALLES
          move AUX-FILE-NAME to FILE-NAME
          move 'D'           to FILE-TYPE
          perform ENVIA-ARCHIVO

          if LAST-KEY = OK
             perform ABRE-REF-CLI-NUEV
             move AUX-FILE-NAME to FILE-NAME
             move 'D'           to FILE-TYPE
             perform ENVIA-ARCHIVO

             if LAST-KEY = OK
                perform ABRE-REF-SO-ABAST
                move AUX-FILE-NAME to FILE-NAME
                move 'D'           to FILE-TYPE
                perform ENVIA-ARCHIVO
             end-if
          end-if
       end-if
   end-if

   clear
   if LAST-KEY = ER
      fill CMD-REC-WS with ' '
      move '81' to CMD                ;Status Malo!!
      perform ENVIA-COMANDO-HOST
   end-if

   perform DESCONECTA-HOST
   perform PREGUNTA-SI-BORRA

exit.

PREGUNTA-SI-BORRA.
;-----------------

    beep
    message 1 1 'Borrar  Archivos'
    message 1 2 '    ENVIADOS    '
    message 1 3 ' ***************'
    message 1 4 '   [SI]  [NO]   '

    ACTION

    when NO    exit.
    when ENTER perform ABRE-UPD-CABECERA
               ERASE CABECERA-F

               perform ABRE-UPD-DETALLES
               ERASE DETALLES-F

               perform ABRE-UPD-SO-ABAST
               ERASE SO-ABAST-F

               perform ABRE-UPD-CLI-NUEV
               ERASE CL-NUEVO-F

               beep
               message 1 1 '****************'
               message 1 2 '    ARCHIVOS    '
               message 1 3 '   ELIMINADOS   '
               message 1 4 '****************'
               wait 1 second
    end-when
loop.

SUPE-IDENTIFICAC.
;----------------
   perform ABRE-REF-IDENTIFI
   seek FIRST in IDENTIFI-F
   when ER  beep
            message 1 1 '****************'
            message 1 2 '   No Existe    '
            message 1 3 ' IDENTIFICACION '
            message 1 4 '****************'
            wait 1 second
   end-when
   when EOF beep
            message 1 1 '****************'
            message 1 2 '     Vacio      '
            message 1 3 ' IDENTIFICACION '
            message 1 4 '****************'
            wait 1 second
   end-when

   when OK read IDENTIFI-F

           move IDE-VENDED to WW-IDE-VENDED
           move IDE-NOMBRE to WW-IDE-NOMBRE
           move IDE-BODEGA to WW-IDE-BODEGA
           move IDE-PJ-IVA to WW-IDE-PJ-IVA
           move IDE-PJ-ILA to WW-IDE-PJ-ILA
           move IDE-POR-IN to WW-IDE-POR-IN

           move IDE-CON-IN to WW-IDE-CON-IN
           move IDE-VISITA to WW-IDE-VISITA
           move IDE-FAC-IN to WW-IDE-FAC-IN
           move IDE-FAC-FI to WW-IDE-FAC-FI
           move IDE-FAC-EM to WW-IDE-FAC-EM
           subtract 1 from  WW-IDE-FAC-EM

           perform SHOW-IDENT-1
   end-when
exit.

SHOW-IDENT-1.
;------------
    show PNT-IDENT-1-sd
    ACTION

    when FAR  perform SHOW-IDENT-2
              when KEND exit
    end-when
    when KEND exit.
loop.

SHOW-IDENT-2.
;------------
    show PNT-IDENT-2-sd
    ACTION

    when BAR  exit.
    when KEND exit.
    when FAR  perform SHOW-IDENT-3
              when KEND exit
    end-when
loop.

SHOW-IDENT-3.
;------------
    show PNT-IDENT-3-sd
    ACTION

    when BAR  exit.
    when KEND exit.
loop.

SUPE-FECHA-HORA.
;---------------
    perform LEE-FECHA-HORA

    move DIA-WS  to WW-DIA
    move MES-WS  to WW-MES
    move ANO-WS  to WW-ANO
    move HORA-WS to WW-HOR
    move MINU-WS to WW-MIN

    show WW-DIA-sd
    show WW-MES-sd
    show WW-ANO-sd
    show WW-HOR-sd
    show WW-MIN-sd

    message 1 3 '    Modifica    '
    message 1 4 '   [SI]  [NO]   '

    action

    when ENTER perform CAMBIA-FECHA.
    when NO    exit.
    when KEND  exit.
loop.

CAMBIA-FECHA.
;------------
    clear
    show WW-HOR-sd
    show WW-MIN-sd

    accept WW-DIA-sd
    when OK accept WW-MES-sd
            when OK accept WW-ANO-sd
                    when OK beep
                            message 1 3 ' Esta Correcto ?'
                            message 1 4 '   [SI]  [NO]   '
                            perform LEE-TECLA

                            when ENTER perform CAMBIA-HORA, exit.
                    end-when
            end-when
    end-when
    when KEND exit.
loop.

CAMBIA-HORA.
;-----------
    clear
    show WW-DIA-sd
    show WW-MES-sd
    show WW-ANO-sd

    accept WW-HOR-sd
    when OK accept WW-MIN-sd
            when OK beep
                    message 1 3 ' Esta Correcto ?'
                    message 1 4 '   [SI]  [NO]   '
                    perform LEE-TECLA

                    when ENTER perform MOD-FECHA-HORA, exit.
            end-when
    end-when
    when KEND exit.
loop.

MOD-FECHA-HORA.
;--------------
    move WW-DIA to DIA-WS
    move WW-MES to MES-WS
    move WW-ANO to ANO-WS
    move WW-HOR to HORA-WS
    move WW-MIN to MINU-WS

    move FECHA-NORMAL      to FECHA-EMPAQUETADA
    move FECHA-EMPAQUETADA to SY-DATE-TIME
    SET-CLOCK
exit.

;*************************************************************************
;*                      ENVIO Y RECIBO DE ARCHIVOS                       *
;*************************************************************************

;***********************
;* RUTINAS UTILITARIAS *
;***********************

LEE-FECHA-HORA.
;--------------
    READ-CLOCK
    move SY-DATE-TIME      to FECHA-EMPAQUETADA
    move FECHA-EMPAQUETADA to FECHA-NORMAL

    if ( HORA-WS < 12 ) and ( AMPM-WS = 1 )  ; 1 = PM , 0 = AM
       add 12 to HORA-WS
       move 1 to FORM-1224-WS

       move FECHA-NORMAL      to FECHA-EMPAQUETADA
       move FECHA-EMPAQUETADA to SY-DATE-TIME
       SET-CLOCK
       loop
    end-if

exit.


LEE-TECLA.
;--------
   FAST-ACTION
EXIT.

AVA-INDICE.
;---------
   IF INDICE >= MAX-INDICE
      MOVE MIN-INDICE TO INDICE
   ELSE
      ADD 1 TO INDICE.
EXIT.

RET-INDICE.
;---------
   IF INDICE <= MIN-INDICE
      MOVE MAX-INDICE TO INDICE
   ELSE
      SUBTRACT 1 FROM INDICE.
EXIT.

;**************************************
;* RUTINAS ASOCIADAS A COMUNICACIONES *
;**************************************

Conecta-Host.
;-----------
   MOVE 1 TO USER-TMOUT.
   MOVE 150 TO GP-TIMER.
   DISCONNECT.
   clear
   message 17, ' ESPERANDO  DSR '
   perform conecta-100.
   move 600 to user-tmout
EXIT.

conecta-100.
;-----------
    COMM-IN DSR.
       WHEN OK perform CONECTA-200
               EXIT.
       when er MOVE GP-TIMER TO GP-TIMER-WS
               SHOW GP-TIMER-SD
               ACTION
               WHEN KEND  MOVE ER TO LAST-KEY
                          EXIT.
               when tmout if gp-timer = 0
                             move 150 to gp-timer
                             power-off
                          else
                             loop
                          end-if.
       end-when
exit.

conecta-200.
;-----------
    connect host-com-96.
    when ER move 'DSR no recibido' to MSG-TXT
            perform mensaje
            exit.
    perform envia-sign-on
    if last-key = ER perform mensaje.
exit.

DESCONECTA-HOST.
;--------------
   if LAST-KEY =  OK
      perform ENVIA-SIGN-OFF
      when ER perform MENSAJE.
   end-if
   DISCONNECT
exit.

Envia-Archivo.
;------------
    if file-type = 'W' or file-type = 'X' or file-type = 'Y' or file-type = 'Z'
       exit.
    move FILE-TYPE to ARCHIVO-F:FD-TYPE
    open Ref ARCHIVO-F
    when ER move 'Archivo no  esta' to MSG-TXT
            perform Mensaje
            move ok to last-key
            exit.
    seek first ARCHIVO-F

         when eof move 'Archivo sin dato' to MSG-TXT
                  perform Mensaje
                  move ok to last-key
                  exit.

    perform Calc-Largo

    fill cmd-file-name-ws with ' '
    move FILE-TYPE     to s-extension
    move '.'           to s-punto
    move FILE-NAME     to arch-a-enviar
    move file-to-send  to filename
    move '03'          to CMD ; envio de archivo
    show ARCH-A-ENVIAR-sd
    perform envia-comando-host
            when ER exit.
    send-etx ARCHIVO-F
             when ER fill msg-comm with ' '
                     move 'ERROR ' TO mens-com-1
                     move com-err  to mens-com-2
                     examine COM-ERR
                     when  1 move ' ERROR DE PARIDAD' to MENS-COM-3.
                     when  2 move ' MUCHOS CARACTERES RECIBIDOS' to MENS-COM-3.
                     when  3 move ' DSR PERDIDO, DESCONECTADO' to MENS-COM-3.
                     when  4 move ' ERROR BCC' to MENS-COM-3.
                     when  5 move ' ERROR EN PRIMER CARACTER O NO DATOS RECIBIDOS' to MENS-COM-3.
                     when  6 move ' ERROR EN ULTIMO CARACTER' to MENS-COM-3.
                     when  7 move ' COLISION EN BUS DE DATOS' TO MENS-COM-3.
                     when  8 move ' ERROR DE OVERRUN' TO MENS-COM-3.
                     when  9 move ' ERROR DE FRAMING' to MENS-COM-3.
                     when 10 move ' DEMASIADOS REINTENTOS (ENQ, NAK, ETC)' TO MENS-COM-3.
                     perform Mensaje
                     move ER to LAST-KEY
             exit.
    perform recibe-comando-host
    BEEP
exit.

envia-sign-on.
;------------
    fill cmd-rec-ws with ' '.
    move '01' to cmd.              ; Comando Sign On
    move '00' to status.           ; Estado OK
    move 'x' to terminal           ; Identificacion de PTC, como 701X
    bid.
        when ER move 'Error BID en Sign-ON' to MSG-TXT
                exit.
    send-etx cmd-rec-ws.
              when ER move 'Error envio ETX en Sign ON' to MSG-TXT
                      exit.
    perform recibe-comando-host
            if last-key = ER perform mensaje
                             move 'Error Recepcion OK sign on' to MSG-TXT.
exit.

envia-sign-off.
;-------------
    fill CMD-REC-WS with ' '.
    move '02' to cmd.
    move '00' to status.
    move 'x' to terminal.
    bid.
        when ER move 'Error BID en Sign OFF' to MSG-TXT
                exit.
    SEND-ETX cmd-rec-ws.
              when ER move 'Error envio ETX en Sign OFF' to MSG-TXT
                      exit.
exit.

envia-comando-host.
;-----------------
    bid.
        when ER move 'Erro BID en Envio Comando a Host' to MSG-TXT
                exit.
    send-etx cmd-rec-ws.
              when ER move 'Error ETX en envio comando al Host' to MSG-TXT.
exit.

recibe-comando-host.
;-------------------
    wait-for-bid.
              when ER move 'ENQ no recibido desde Host' to MSG-TXT
                      exit.
    fill cmd-rec-ws with ' '.
    receive-block cmd-rec-ws.
              when ER    move 'Error al Recibir comando desde Host' to MSG-TXT
                         exit.
              when mem   move 'Memoria Llena !!' to MSG-TXT
                         move ER to last-key
                         exit.
              when extin move 'ERROR bloque ETB recibido OK' to MSG-TXT
                         move ER to last-key
                         exit.
    if cmd ne '80'
       then move 'Error Status al recibir comando' to MSG-TXT
            move ER to last-key
       else move OK to last-key.
exit.

Rec-Archivo.
;----------
    CLEAR
    MESSAGE 1 3 ' Recibiendo <-- '
    fill cmd-file-name-ws with ' '
    move file-to-receive  to filename
    move '10'          to CMD ; recepcion de archivo

    perform envia-comando-host
            when ER exit.

    perform recibe-comando-host
    when ER move ARCH-A-RECIBIR to MSG-COMM
            move 'ARCHIVO NO EXISTE EN HOST...' to mens-com-3
            perform Mensaje
            move er to last-key
            exit.
    perform recibe-comando-host
    if last-key = er and cmd <> '03' exit.
    if cmd = '03' ; PC esta listo para enviar archivo
       show ARCH-A-RECIBIR-sd
       perform abreup-archivo
       perform rec-block           ; recibe archivo en bloques
       when ER exit.
       fill CMD-REC-WS with ' '
       move '80' to CMD
       perform envia-comando-host
       when ER exit.
       perform calc-largo
       show largo-arch-sd
       wait 15 tenths
       BEEP
    end-if
exit.

rec-block.
;--------
       receive-block ARCHIVO-F
             when EXTIN beep
                        loop.
             when mem beep
                      clear
                      message 17 ' Falta Memoria  '
                      message 33 '     en PTC     '
                      perform lee-tecla
                      move er to last-key
                      exit.
             when ER fill msg-comm with ' '
                     move 'ERROR ' TO mens-com-1
                     move com-err  to mens-com-2
                     examine COM-ERR
                     when 1 move ' ERROR DE PARIDAD' to MENS-COM-3.
                     when 2 move ' MUCHOS CARACTERES RECIBIDOS' to MENS-COM-3.
                     when 3 move ' DSR PERDIDO, DESCONECTADO' to MENS-COM-3.
                     when 4 move ' ERROR BCC' to MENS-COM-3.
                     when 5 move ' ERROR EN PRIMER CARACTER O NO DATOS RECIBIDOS' to MENS-COM-3.
                     when 6 move ' ERROR EN ULTIMO CARACTER' to MENS-COM-3.
                     when 7 move ' COLISION EN BUS DE DATOS' TO MENS-COM-3.
                     when 8 move ' ERROR DE OVERRUN' TO MENS-COM-3.
                     when 9 move ' ERROR DE FRAMING' to MENS-COM-3.
                     when 10 move ' DEMASIADOS REINTENTOS (ENQ, NAK, ETC)' TO MENS-COM-3.
                     perform Mensaje
                     move ER to LAST-KEY.
exit.

AbreUp-Archivo.
;--------------
   MOVE ARCH-A-RECIBIR TO FILE-NAME
   MOVE R-EXTENSION    TO ARCHIVO-F:FD-TYPE
   open update ARCHIVO-F
   when ER create ARCHIVO-F
           loop.
   reset ARCHIVO-F
exit.

Mensaje.
;-------
show Linea-MSG.
wait 3 second.
clear 4.
Exit.

Calc-Largo.
;---------
   move archivo-f:end-pointer to largo-arch
   subtract archivo-f:begin-pointer from largo-arch
Exit.


;******************************************************************
;*         INICIO-INICIO-RUTINAS ASOCIADAS A DECLARATIVES         *
;******************************************************************

POWER-MESSAGE.
;------------
    move DISPLAY-BUFFER to RESP-PANTALLA
    message 1 1 '   AUTOVENTA    '
    message 1 2 '  DEMOSTRACION  '
    message 1 3 '                '
    message 1 4 '    Ver.1.0     '
    wait 1 second
    move RESP-PANTALLA to DISPLAY-BUFFER
exit.

CHECK-MEMORY.                                   ; Rutina Muestra Memoria
;------------                                   ; Disponible
    move DISPLAY-BUFFER to DISP-SAVE
    if  2000 < MEM-AVAIL
        then move 2000 to MEM-PROTECT
             clear message 1, ' Mem Disponible '
             MOVE MEM-AVAIL TO WS-MEM-AV
             show MEM-REM-S
        else if 200 < MEM-AVAIL
                then move 200 to MEM-PROTECT
                     clear message 1, ' Falta  Memoria '
                     MOVE MEM-AVAIL TO WS-MEM-AV
                     show MEM-REM-S
                     beep
                else move 200 to MEM-PROTECT
                     clear message 1, ' Memoria  Llena '
                     MOVE MEM-AVAIL TO WS-MEM-AV
                     show MEM-REM-S.
    end-if
    wait 1 second.
    move DISP-SAVE to DISPLAY-BUFFER.
    exit.

SHOW-RELOJ.
;----------

    perform LEE-FECHA-SISTEMA

    move DIA-WS  to WW-DIA
    move MES-WS  to WW-MES
    move ANO-WS  to WW-ANO
    move HORA-WS to WW-HOR
    move MINU-WS to WW-MIN

    perform SHOW-LA-FECHA

exit.

SHOW-LA-FECHA.
;-------------

    move DISPLAY-BUFFER to RESP-PANTALLA

    clear
    show WW-DIA-sd
    show WW-MES-sd
    show WW-ANO-sd
    show WW-HOR-sd
    show WW-MIN-sd

    message 1 4 '  Fecha - Hora  '

    wait 1 second

    move RESP-PANTALLA to DISPLAY-BUFFER
exit.

DECLA-RELOJ.
;-----------
    perform MOST-RELOJ
exit.

DEC-RELOJ.
;---------
   CLEAR 2 - 4
   if IND-TECLA-FN = 0
      move 1 to IND-TECLA-FN
      perform MOST-RELOJ            ; Muestra Reloj
      when OK exit.
      move 0 to IND-TECLA-FN
   end-if
LOOP.

MOST-RELOJ.
;---------
   MOVE DISPLAY-BUFFER TO display-save
   PERFORM LEE-FECHA-SISTEMA
   PERFORM MUESTRA-FECHA
   MOVE 9 TO USER-TMOUT
   PERFORM MUESTRA-HORA
   MOVE 600 TO USER-TMOUT
   move display-save to DISPLAY-BUFFER
EXIT.

MUESTRA-FECHA.
;-------------
    SHOW FECHA-SD.
EXIT.

MUESTRA-HORA.
;------------
   PERFORM LEE-FECHA-SISTEMA
   SHOW HORA-SD
   ACTION
   WHEN TMOUT LOOP.
   when ENTER MOVE OK TO LAST-KEY.
   when Kmod Move 600 to USER-TMOUT
             Perform Mod-Fecha
             Perform Mod-Hora
             Move 9 to USER-TMOUT
             move OK to last-key
             loop.
EXIT.

Mod-Fecha.
;---------
   move 0 to Lectura-Valida.
   Perform Lee-Valida-Fecha Until Lectura-Valida = 1.
   Show Fecha-SD.
   Message 49, "OK ?  ".
   fast-Action
   when ENTER  message 17, " Fecha:"
               Perform Carga-Fecha
               move OK to last-key.
   when NO  loop.
   Exit.

Lee-Valida-Fecha.
;-----------------
   Accept Fecha-SD.
   Accept Fecha2-SD.
   Accept Fecha3-SD.
   move 0 to Lectura-Valida.
;   move mes-ws to mes-hoy
   move 1 to Lectura-Valida.
Exit.

Mod-Hora.
;--------
   Accept Hora-SD.
   Accept Hora2-SD.
   Accept Hora3-SD.
   clear 2.
   Show Fecha-SD.
   clear 3.
   Show Hora-SD.
   Message 49, 'OK ?  '.
   fast-Action.
   clear 3.
   When ENTER CLEAR 4
              Perform Carga-Fecha
              move OK to last-key.
   when NO  loop.
Exit.

Carga-Fecha.
   if form-1224-ws <> 1
      move 1 to form-1224-ws.
   if ( hora-ws >= 12 ) and ( ampm-ws = 0 )
      move 1 to ampm-ws.
   move Fecha-Normal to Fecha-Empaquetada
   move Fecha-Empaquetada to SY-DATE-TIME.
   set-clock.
exit.

LEE-FECHA-SISTEMA.
;-----------------
   READ-CLOCK.
   MOVE SY-DATE-TIME TO FECHA-EMPAQUETADA
   MOVE FECHA-EMPAQUETADA TO FECHA-NORMAL
   IF ( HORA-WS < 12 ) AND ( AMPM-WS = 1 )
        ADD 12 to HORA-WS
        MOVE 1 to FORM-1224-WS
        MOVE FECHA-NORMAL TO FECHA-EMPAQUETADA
        MOVE FECHA-EMPAQUETADA TO SY-DATE-TIME
        SET-CLOCK.
EXIT.

DEC-LUZ.
;-------
   if flag-hora = 0 then
      backlight-on 20 seconds
      move 1 to flag-hora
   else
      backlight-off
      move 0 to flag-hora
   end-if.
EXIT.

;*********************************************************************
;*          FIN-FIN DE RUTINAS ASOCIADOS A LA DECLARATIVES           *
;*********************************************************************


ABRE-REF-IDENTIFI.
;-----------------
    move 'IDENTIFI' to FILE-NAME
    open REF IDENTIFI-F
exit.

ABRE-REF-CLI-CART.
;-----------------
    move 'CL-CARTE'    to FILE-NAME
    open REF CL-CARTE-F
exit.

ABRE-REF-CLI-RUTA.
;-----------------
    move 'CL-RUTA'    to FILE-NAME
    open REF CL-RUTA-F
exit.

ABRE-REF-CLI-NUEV.
;-----------------
    move 'CLI'         to AUX-F-N-PREF
    move W-IDE-VENDED  to AUX-F-N-VEND
    move AUX-FILE-NAME to FILE-NAME

    open REF CL-NUEVO-F
exit.

ABRE-UPD-CLI-NUEV.
;-----------------
    move 'CLI'         to AUX-F-N-PREF
    move W-IDE-VENDED  to AUX-F-N-VEND
    move AUX-FILE-NAME to FILE-NAME

    open UPDATE CL-NUEVO-F
    when ER create CL-NUEVO-F, loop.
exit.

ABRE-REF-CONDICIO.
;-----------------
    move 'COND-VTA' to FILE-NAME
    open REF COND-VTA-F
exit.

ABRE-REF-PRODUCTO.
;-----------------
    move 'PRODUCTO'    to FILE-NAME
    open REF PRODUCTO-F
exit.

ABRE-REF-EXISTENC.
;-----------------
     move 'EX-BODEG'    to FILE-NAME
     open REF EX-BODEG-F
exit.

ABRE-UPD-SO-ABAST.
;-----------------
    move 'SOL'         to AUX-F-N-PREF
    move W-IDE-VENDED  to AUX-F-N-VEND
    move AUX-FILE-NAME to FILE-NAME

    open UPDATE SO-ABAST-F
    when ER create SO-ABAST-F, loop.
exit.

ABRE-REF-SO-ABAST.
;-----------------
    move 'SOL'         to AUX-F-N-PREF
    move W-IDE-VENDED  to AUX-F-N-VEND
    move AUX-FILE-NAME to FILE-NAME

    open REF SO-ABAST-F
exit.

ABRE-UPD-DETALLES.
;-----------------
    move 'DET'         to AUX-F-N-PREF
    move W-IDE-VENDED  to AUX-F-N-VEND
    move AUX-FILE-NAME to FILE-NAME

    open UPDATE DETALLES-F
    when ER create DETALLES-F, loop.
exit.

ABRE-REF-DETALLES.
;-----------------
    move 'DET'         to AUX-F-N-PREF
    move W-IDE-VENDED  to AUX-F-N-VEND
    move AUX-FILE-NAME to FILE-NAME

    open REF DETALLES-F
exit.

ABRE-UPD-CABECERA.
;-----------------
    move 'ENC'         to AUX-F-N-PREF
    move W-IDE-VENDED  to AUX-F-N-VEND
    move AUX-FILE-NAME to FILE-NAME

    open UPDATE CABECERA-F
    when ER create CABECERA-F, loop.
exit.

ABRE-REF-CABECERA.
;-----------------
    move 'ENC'         to AUX-F-N-PREF
    move W-IDE-VENDED  to AUX-F-N-VEND
    move AUX-FILE-NAME to FILE-NAME

    open REF CABECERA-F
exit.






















