;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ              I D E N T I F I C A T I O N     D I V I S I O N               ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
IDENTIFICATION DIVISION.
program-id.             'EXCEL'.
author.                 Hern n Valds C., Alvaro Medina G. y Alvaro Sandoval P.
installation.           RIMPEX-CHILE / CERVESUR S.A.
date-written.           28/05/93.
date-compiled.          08/06/94.
remarks.

   ;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
   ;ณ           CERVECERIA DORADA S.A.  Sistema de Autoventa para        ณ
   ;ณ                          Arequipa - Perฃ                           ณ
   ;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                            ENVIRONMENT DIVISION                            ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
source-computer.    PC.
object-computer.    T710.
special-names.

        KEY  NO      IS 1.
        KEY  PROC    IS 2.
        KEY  ADJ     IS 3.
        KEY  KMOD    IS 9.
        KEY  KNEXT   IS 10.
        KEY  KINS    IS 11.
        KEY  YES     IS 12.
        KEY  KBEEP   IS 13.
        KEY  KNOTA   IS 14.
        KEY  KEXIT   IS 15.
        KEY  KELIM   IS 16.
        KEY  KMENU   IS 17.
        KEY  KFIND   IS 18.
        KEY  KMEM    IS 19.
        KEY  KVEND   IS 20.
        KEY  KQUIT   IS 21.
        KEY  KVER    IS 22.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                                DATA DIVISION                               ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
DATA DIVISION.
FILE SECTION.

FD BUSCA-I
   recording mode is free
   type is 'I'.

                     ;ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท;
                     ;บ       ARCHIVOS DE ENTRADA       บ;
                     ;ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ;
;-----------------------------------------------------------------------------

FD MAE-CLI-F                               ; Maestro de CLIENTES
   recording mode is fixed
   type is 'D'.

01 REG-MAE-CLI.
   03 codigo-mae-cli     pic 9(06).        ; Cขdigo empresa
   03 ruc-mae-cli.
      05 ruc-serie       pic 9(07).        ; Ruc del Cliente
      05 dig-veri        pic X(01).        ; Digito Verificador
   03 n-estb-mae-cli     pic X(32).        ; Nombre Establecimiento
  ;03 n-prop-mae-cli     pic X(32).        ; Nombre Propietario
   03 direcc-mae-cli     pic X(35).        ; Direccion Cliente
   03 ruta-mae-cli       pic 9(04).        ; Nฃmero de Ruta asignada al Cliente

;-----------------------------------------------------------------------------

FD CTR-IMP-F                               ; Control de Impresora
   recording mode is fixed
   type is 'D'.

01 REG-CTR-IMP.
   03 flag-impresora     pic X(01).        ; Flag Impresora (S/N)

;-----------------------------------------------------------------------------

FD MAE-PRO-F                               ; Maestro de PRODUCTOS
   recording mode is fixed
   type is 'D'.

01 REG-MAE-PRO.
   03 codigo-mae-pro     pic 9(03).        ; Cขdigo Producto
   03 descri-mae-pro     pic X(16).        ; Descripciขn Producto
   03 precio-mae-pro     pic 9(06)V99.     ; Precio de Venta Incluido I.G.V.
   03 nround-mae-pro     pic 9(02).        ; Nro. Unidades.
   03 relac1-mae-pro     pic 9(03).        ; Relaciขn 01 (CPG)
   03 relac2-mae-pro     pic 9(03).        ; Relacion 02 (CPC)
   03 envase-mae-pro     pic 9(01).        ; Prstamos de Envases.
   03 consig-mae-pro     pic 9(01).        ; Consignaciones

;-----------------------------------------------------------------------------

FD TAB-RUT-F                               ; Tabla de Visitas de la Ruta
   recording mode is fixed
   type is 'D'.

01 REG-TAB-RUT.
   03 codigo-tab-rut     pic 9(04).        ; Cขdigo de Ruta
   03 dias-tab-rut.                        ; Dกas de Visitas
      05 lun-tab-rut     pic 9(01).
      05 mar-tab-rut     pic 9(01).
      05 mie-tab-rut     pic 9(01).
      05 jue-tab-rut     pic 9(01).
      05 vie-tab-rut     pic 9(01).
      05 sab-tab-rut     pic 9(01).

;-----------------------------------------------------------------------------

FD ARC-COB-F                               ; Archivo de COBRANZAS
   recording mode is fixed                 ;            PENDIENTES
   type is 'D'.

01 REG-ARC-COB.
   03 codigo-arc-cob     pic 9(06).        ; Cขdigo del Cliente
   03 nr-doc-arc-cob.                      ; Nฃmero de Factura
      05 doc-ref-cob     pic X(08).
      05 bofa-cob        pic X(01).
   03 fec-e-arc-cob      pic 9(06).        ; Fecha de Emisiขn
   03 fec-v-arc-cob      pic 9(06).        ; Fecha de Vencimiento
   03 m-ade-arc-cob      pic 9(06)V99.     ; Monto Adeudado

;-----------------------------------------------------------------------------

FD ARC-INV-F                               ; Archivo de INVENTARIO
   recording mode is fixed
   type is 'S'.

01 REG-ARC-INV.
   03 codigo-arc-inv     pic 9(03).        ; Cขdigo del Producto
   03 ini-arc-inv        pic 9(08).        ; Stock Inicial Stock/Camiขn
   03 canti-arc-inv      pic 9(08).        ; Cantidad en Stock/Camiขn
   03 vco-arc-inv        pic 9(08).        ; Ventas Realizadas
   03 vcr-arc-inv        pic 9(08).        ; Ventas al Crdito.
   03 pen-arc-inv        pic 9(08).        ; Prstamos
   03 den-arc-inv        pic 9(08).        ; Devoluciones
;  03 rut-arc-inv        pic 9(04).        ; Ruta
   03 fec-arc-inv        pic 9(06).        ; Fecha
   03 ven-arc-inv        pic 9(02).        ; Vendedor
   03 cso-arc-inv        pic 9(08).        ; Consignaciones Otorgadas
   03 csr-arc-inv        pic 9(08).        ; Consignaciones Recogidas
   03 recarga-inv        pic 9(08).        ; Recargas.
   03 obs-arc-inv        pic 9(08).        ; Obsequios.
   03 dsc-arc-inv        pic 9(08).        ; Descargas.
   03 cjs-arc-inv        pic 9(08).        ; Canjes
   03 rts-arc-inv        pic 9(08).        ; Roturas
   03 cr-inv             pic 9(01) Binary. ; Retorno de Carro.
   03 lf-inv             pic 9(01) Binary. ; Line Feed.

;-----------------------------------------------------------------------------

FD IDENTIFICA-F                            ; Nombre Archivo IDENTIFICACION
   recording mode is fixed                 ; IDENTI+Codigo-Vendedor.I
   type is 'I'.                            ; (Archivo a cargar diariamente)

01 REG-IDENTIFICA.
   03 cod-arc-ide           pic X(02).     ; Codigo Vendedor
   03 nom-arc-ide           pic X(16).     ; Nombre Vendedor
   03 password-ide.                        ; Password de encendido
      05 menu-arc-ide       pic 9(01).     ; switch tipo menu : 0=Vendedor, 1=Supervisor, 2=Operador, 3=Almacenero
      05 pass-arc-ide       pic X(06).     ; Clave de Entrada

;-----------------------------------------------------------------------------
FD DET-ENV-F                               ; Archivo de ENVASES
   recording mode is fixed                 ; PENDIENTES
   type is 'D'.

01 REG-DET-ENV.
   03 codigo-det-env     pic 9(06).        ; Cขdigo del Cliente
   03 nro-doc-det-env    pic X(08).        ; Nฃmero de Documento
   03 cod-pro-det-env    pic 9(03).        ; Cขdigo del Envase
   03 can-pre-det-env    pic 9(08).        ; Cantidad Prestada
   03 fecha-det-env      pic 9(06).        ; Fecha de Emisiขn

;-----------------------------------------------------------------------------
FD PEDIDOS-F                               ; Archivo de ENVASES
   recording mode is fixed                 ; PENDIENTES
   type is 'D'.

01 REG-PEDIDOS.
   03 codigo-ped         pic 9(06).        ; Cขdigo del Cliente
   03 nro-doc-ped        pic X(08).        ; Nฃmero de Documento
   03 cod-pro-ped        pic 9(03).        ; Cขdigo del Envase
   03 can-pre-ped        pic 9(08).        ; Cantidad Prestada
   03 fecha-ped          pic 9(06).        ; Fecha de Emisiขn


;ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท
;บ          Archivos Entrada de Consignaciones                               บ
;ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ
FD DET-CSG-F                               ; Archivo de CONSIGNACIONES
   recording mode is fixed                 ; PENDIENTES
   type is 'D'.

01 REG-DET-CSG.
   03 codigo-det-csg     pic 9(06).        ; Cขdigo del Cliente
   03 nro-doc-det-csg    pic X(08).        ; Nฃmero de Documento
   03 cod-pro-det-csg    pic 9(03).        ; Cขdigo del Envase
   03 can-pre-det-csg    pic 9(08).        ; Cantidad Prestada
   03 fecha-det-csg      pic 9(06).        ; Fecha de Emisiขn
   03 Env-det-csg        pic 9(01).        ; Con o sin envases.

;-----------------------------------------------------------------------------
FD ENV-CSG-F                               ; Archivo de CONSIGNACIONES
   recording mode is fixed                 ; PENDIENTES DE ENVASES
   type is 'D'.                            ;

01 REG-ENV-CSG.
   03 codigo-env-csg     pic 9(06).        ; Cขdigo del Cliente
   03 cod-pro-env-csg    pic 9(03).        ; Cขdigo del Envase
   03 can-pre-env-csg    pic 9(08).        ; Cantidad Prestada
   03 fecha-env-csg      pic 9(06).        ; Fecha de Emisiขn
 ; 03 nro-doc-env-csg    pic X(08).        ; Nฃmero de Documento

;-----------------------------------------------------------------------------
FD ENV-ADE-F                               ; Archivo de CONSIGNACIONES
   recording mode is fixed                 ; PENDIENTES DE ENVASES
   type is 'D'.                            ; ADELANTADOS.

01 REG-ENV-ADE.
   03 codigo-env-ade     pic 9(06).        ; Cขdigo del Cliente
   03 nro-doc-env-ade    pic X(08).        ; Nฃmero de Documento
   03 cod-pro-env-ade    pic 9(03).        ; Cขdigo del Envase
   03 can-pre-env-ade    pic 9(08).        ; Cantidad Prestada
;-----------------------------------------------------------------------------


;-----------------------------------------------------------------------------
FD TAB-MOT-F                               ; Tabla de MOTIVOS DE NO COMPRA
   recording mode is fixed
   type is 'D'.

01 REG-TAB-MOT-F.
   03 ncomp-codigo       pic 9(02).        ; Cขdigo del motivo
   03 ncomp-descripcion  pic X(16).        ; descripciขn motivo
   03 ncomp-atendido     pic 9(01).        ; 0=No Atendido, 1=Atendido

;-----------------------------------------------------------------------------

FD TAB-FOL-F                               ; Tabla de Folios (Correlativos)
   recording mode is fixed                 ; de Documentos
   type is 'D'.

01 REG-TAB-FOL.
   03 codigo-tab-fol     pic 9(02).        ; Cขdigo de Tipo Documento
   03 folio-tab-ini      pic 9(08).        ; Numero Inicial.
   03 folio-tab-fol      pic 9(08).        ; 01 = Facturas.
                                           ; 02 = Prestamos
                                           ; 03 = Devoluciones
                                           ; 04 = Cobranzas.
                                           ; 05 = Canjes
                                           ; 06 = Varios
                                           ; 07 = Obsequios
                                           ; 08 = Entrega de Dinero
                                           ; 09 = Descargas
                                           ; 10 = Boletas
                                           ; 11 = Recarga en Ruta.
                                           ; 12 = Rotura.
                                           ; 13 = Gastos.
                                           ; 14 = Consig. Otorgadas
                                           ; 15 = Consig. Recogidas.
                                           ; 16 = Recojo de Envases Csg.
                                           ; 17 = Devoluciขn de Envases Csg.


   03 folio-tab-dsc      pic X(16).

;-----------------------------------------------------------------------------

FD EMBOTELLADORA-F                       ; Nombre Archivo EMBOTELLADORAS
   recording mode is fixed               ; EMBOTE+Codigo-Vendedor.E
   type is 'D'.

01 REG-EMBOTELLADORA.
   03 embo-codigo        pic 9(03).      ; Codigo embotelladora
   03 embo-empresa       pic X(16).      ; Nombre empresa

;-----------------------------------------------------------------------------

FD PRODUC-CENSO-F                        ; Nombre Archivo PRODUCTOS PARA CENSO
   recording mode is fixed               ; PCENSO+Codigo-Vendedor.E
   type is 'D'.

01 REG-PRODUC-CENSO.
   03 prce-codigo.
      05 prce-cod1       pic 9(03).      ; Codigo empresa embotelladora
      05 prce-cod2       pic 9(05).      ; Codigo Producto
   03 prce-descripcion   pic X(16).      ; Descripcion del Tamaคo

;-----------------------------------------------------------------------------

FD ITEM-PUBLICID-F                       ; Archivo de Publicidad.
   recording mode is fixed
   type is 'D'.

01 REG-ITEM-PUBLICID.
   03 IP-COD-ITEM        PIC X(3).
   03 IP-DESCRIP         PIC X(35).

;-----------------------------------------------------------------------------

FD SISTEMA-F                             ; Datos Generales de la Empresa.
   recording mode is fixed
   type is 'D'.

01 REG-SISTEMA.
   03 SIS-NOMBRE-CIA.
      05 SIS-NOM-01      pic X(22).
      05 SIS-NOM-02      pic X(13).
   03 SIS-DIRECC-CIA.
      05 SIS-DIR-01      pic X(20).
      05 SIS-DIR-02      pic X(15).
   03 SIS-RUC-CIA        pic 9(08).
   03 SIS-COD-CIA.
      05 SIS-COD-01      pic X(03).
      05 SIS-COD-02      pic X(02).

FD RAMOS-F                               ; Nombre Archivo RAMOS
   recording mode is fixed               ; RA+Codigo-Vendedor.E
   type is 'D'.

01 REG-RAMOS.
   03 ra-codigo          pic 9(02).      ; Codigo ramo
   03 ra-descripcion     pic X(16).      ; Descripcion ramo

FD BANCOS-F                              ; Nombre Archivo BANCOS
   recording mode is fixed               ; RA+Codigo-Vendedor.E
   type is 'D'.

01 REC-BANCOS.
   03 bc-codigo          pic 9(02).      ; Codigo banco
   03 bc-descripcion     pic X(16).      ; Descripcion banco


FD MENSAJE-CLI-F                         ; Nombre Archivo MENSAJE PARA CLIENTES
   recording mode is fixed               ; MC+Codigo-Vendedor.E
   type is 'E'.

01 REG-MENSAJE-CLI.
   03 mc-codcli          pic 9(06).      ; Codigo Cliente
   03 mc-mensaje         pic X(32).      ; Mensaje para el cliente


;-----------------------------------------------------------------------------
                     ;ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท;
                     ;บ       ARCHIVOS DE SALIDA        บ;
                     ;ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ;

FD ARC-ATE-F                               ; Archivo de CLIENTES ATENDIDOS
   recording mode is fixed
   type is 'S'.

01 REG-ARC-ATE.
   03 codigo-arc-ate     pic 9(06).        ; Cขdigo del Cliente
   03 estado-arc-ate     pic 9(01).        ; Estatus del Cliente (1=Atendido, 0=No Atendido)
   03 motivo-arc-ate     pic 9(02).        ; Motivo por que no Comprข
   03 Fecha-arc-ate      pic 9(06).        ; Fecha
   03 HoraI-arc-ate      pic 9(06).        ; HoraI
   03 HoraF-arc-ate      pic 9(06).        ; HoraF
   03 Ruta-arc-ate       pic 9(04).        ; Ruta
   03 Vend-arc-ate       pic 9(02).        ; Vendedor
   03 venta-arc-ate      pic 9(01).        ; (1=Venta, 0=No Venta)
   03 cr-arc-ate         pic 9(01) Binary. ; Retorno de Carro.
   03 lf-arc-ate         pic 9(01) Binary. ; Line Feed.

;-----------------------------------------------------------------------------

FD ARC-NUE-F                               ; Archivo de CLIENTES NUEVOS
   recording mode is fixed
   type is 'S'.

01 REG-ARC-NUE.

   03 codigo-arc-nue     pic 9(06).        ; Cขdigo de Ruta
   03 n-prop-arc-nue     pic X(35).        ; Nombre Propietario
   03 n-estb-arc-nue.                      ; Nombre Establecimiento
      05 n-estb-arc-nue1 pic X(01).
      05 n-estb-arc-nue2 pic X(34).
   03 direcc-arc-nue     pic X(35).        ; Direccion Cliente
   03 telefo-arc-nue     pic 9(06).        ; Telfono Cliente
   03 ruc-arc-nue.                         ; Ruc del Cliente
      05 serie-arc-nue   pic 9(07).
      05 dig-arc-nue     pic X(01).
   03 ruta-arc-nue       pic 9(04).        ; Ruta
   03 frec-arc-nue       pic X(06).        ; Frecuencia de Visita
   03 nego-arc-nue       pic 9(02).        ; Tipo de Negocio
   03 cr-arc-nue         pic 9(01) Binary. ; Retorno de Carro.
   03 lf-arc-nue         pic 9(01) Binary. ; Line Feed.

;-----------------------------------------------------------------------------

FD PAGOSF-F                                ; Archivo PAGOS DE FACTURAS.
   recording mode is fixed
   type is 'S'.

01 REG-PAGOSF.
   05 codigo-pag         pic 9(06).        ; Cขdigo del Cliente
   05 nfac-pag           pic X(08).        ; Nฃmero del Documento
   05 bofa-pag           pic X(01).        ; Boleta o Factura
   03 monto-che-pag      pic 9(06)V99.     ; Monto de Cheque
   03 monto-efe-pag      pic 9(06)V99.     ; Monto de Efectivo
   03 date-pag           pic 9(06).        ; Fecha de la Venta
   03 bco-pag            pic 9(02).        ; Banco
   03 nro-chq-pag        pic X(08).        ; Cheque
   03 hora-pag           pic 9(06).        ; Hora de la Venta
   03 cr-pag             pic 9(01) Binary. ; Retorno de Carro.
   03 lf-pag             pic 9(01) Binary. ; Line Feed.
;-----------------------------------------------------------------------------

FD CFA-VTA-F                               ; Archivo CABECERA DE FACTURA
   recording mode is fixed
   type is 'S'.

01 REG-CFA-VTA.
   03 clave-cfa-vta.
      05 codigo-cfa-vta  pic 9(06).        ; Cขdigo del Cliente
      05 nfac-cfa-vta    pic X(08).        ; Nฃmero del Documento
      05 bofa-cfa-vta    pic X(01).        ; Boleta o Factura
   03 ruta-cfa-vta       pic 9(04).
   03 c-ven-cfa-vta      pic 9(02).        ; Cขdigo del Vendedor
   03 date-cfa-vta.                        ; Fecha de la Venta
      05 dia-cfa-vta     pic 9(02).
      05 mes-cfa-vta     pic 9(02).
      05 ano-cfa-vta     pic 9(02).
   03 tipo-cfa-vta       pic 9(01).        ; Tipo de Venta(1=Contado, 2=Credito)
   03 monto-cfa-vta      pic 9(06)V99.     ; Monto de la Factura
   03 monto-adelanto     pic 9(06)V99.     ; Adelanto
   03 dsc-cfa-vta        pic 9(06)V99.     ; Descuento
   03 igv-cfa-vta        pic 9(06)V99.     ; Impuesto de la Factura.
   03 bco-cfa-vta        pic 9(02).        ; Banco
   03 cheque-cfa-vta     pic X(08).        ; Cheque
   03 ind-adelanto       pic 9(01).        ; 0=no adelanto; 1= cheque 2=efectivo, 3= cheque + Efectivo
   03 time-cfa-vta.                        ; Hora de la Venta
      05 hora-cfa-vta    pic 9(02).
      05 minu-cfa-vta    pic 9(02).
      05 segu-cfa-vta    pic 9(02).
   03 Dias-cfa-vta       pic 9(02).        ; Dias de Crdito.
   03 CsFc-cfa-vta       pic 9(01).        ; 0 = Factura Normal, 1 = Factura por Consignacion.
   03 cr-cfa-vta         pic 9(01) Binary. ; Retorno de Carro.
   03 lf-cfa-vta         pic 9(01) Binary. ; Line Feed.

;-----------------------------------------------------------------------------

FD DFA-VTA-F                               ; Archivo DETALLE DE FACTURA
   recording mode is fixed
   type is 'S'.

01 REG-DFA-VTA.
   03 clave-dfa-vta.
      05 codigo-dfa-vta  pic 9(06).        ; Cขdigo del Cliente
      05 nfac-dfa-vta    pic X(08).        ; Nฃmero del Documento
      05 bofa-dfa-vta    pic X(01).        ; Nฃmero del Documento
   03 cod-pro-dfa-vta    pic 9(03).        ; Cขdigo del Producto
   03 can-pro-dfa-vta    pic 9(08).        ; Cantidad del Producto
   03 pre-uni-dfa-vta    pic 9(08).        ; Precio Unitario
   03 date-dfa-vta.                        ; Fecha de la Venta
      05 dia-dfa-vta     pic 9(02).
      05 mes-dfa-vta     pic 9(02).
      05 ano-dfa-vta     pic 9(02).
   03 acu-pro-dfa-vta    pic 9(08).        ; Acumulado
   03 cr-dfa-vta         pic 9(01) Binary. ; Retorno de Carro.
   03 lf-dfa-vta         pic 9(01) Binary. ; Line Feed.

;-----------------------------------------------------------------------------

FD EVENTUALF-F                             ; Archivo EVENTUALES DE FACTURACION
   recording mode is fixed
   type is 'S'.

01 REG-EVENTUALF.
   03 clave-eve-fc.
      05 codcli-eve-fc   pic 9(06).        ; Cขdigo del Cliente
      05 nfac-eve-fc     pic X(08).        ; Nฃmero del Documento
      05 bofa-eve-fc     pic X(01).        ;
   03 nombre-eve-fc      pic X(35).        ; Nombre del Eventual
   03 direcc-eve-fc      pic X(35).        ; Direccion del Eventual.
   03 cr-eve-fc          pic 9(01) Binary. ; Retorno de Carro.
   03 lf-eve-fc          pic 9(01) Binary. ; Line Feed.

;-----------------------------------------------------------------------------

FD EVENTUALE-F                             ; Archivo EVENTUALES DE
   recording mode is fixed                 ; ENVASES
   type is 'S'.

01 REG-EVENTUALE.
   03 clave-eve-fc.
      05 codcli-eve-ev   pic 9(06).        ; Cขdigo del Cliente
      05 nfac-eve-ev     pic X(08).        ; Nฃmero del Documento
   03 nombre-eve-ev      pic X(35).        ; Nombre del Eventual
   03 direcc-eve-ev      pic X(35).        ; Direccion del Eventual.
   03 cr-eve-ev          pic 9(01) Binary. ; Retorno de Carro.
   03 lf-eve-ev          pic 9(01) Binary. ; Line Feed.

;-----------------------------------------------------------------------------

FD EVENTUALC-F                             ; Archivo EVENTUALES DE
   recording mode is fixed                 ; CONSIGNACIONES.
   type is 'S'.

01 REG-EVENTUALC.
   03 clave-eve-fc.
      05 codcli-eve-cs   pic 9(06).        ; Cขdigo del Cliente
      05 nfac-eve-cs     pic X(08).        ; Nฃmero del Documento
   03 nombre-eve-cs      pic X(35).        ; Nombre del Eventual
   03 direcc-eve-cs      pic X(35).        ; Direccion del Eventual.
   03 cr-eve-cs          pic 9(01) Binary. ; Retorno de Carro.
   03 lf-eve-cs          pic 9(01) Binary. ; Line Feed.

;-----------------------------------------------------------------------------

FD DCO-PAG-F                               ; Archivo DE COBRANZAS DEL DIA.
   recording mode is fixed
   type is 'S'.

01 REG-DCO-PAG.
   03 clave-dco-pag.
      05 codigo-dco-pag  pic 9(06).        ; Cขdigo del Cliente
      05 nfac-dco-pag    pic X(08).        ; Nฃmero de la Factura
   03 ndoc-dco-pag       pic X(09).        ; Documento
   03 monto-dco-pag      pic 9(06)V99.     ; Monto Cancelado
   03 tipo-dco-pag       pic 9(01).        ; Tipo de Pago (1=Efectivo,2=Cheque)
   03 tipo-dco-din       pic 9(01).        ; Tipo de Pago (1=Amortiza,2=Cancela
   03 bco-dco-pag        pic 9(02).        ; Banco
   03 cheque-dco-pag     pic X(08).        ; Cheque
   03 fecha-dco-pag      pic X(06).        ; Fecha de la Cobranza
   03 vend-dco-pag       pic 9(02).        ; Vendedor
   03 ruta-dco-pag       pic 9(04).        ; Ruta
   03 cr-dco-pag         pic 9(01) Binary. ; Retorno de Carro.
   03 lf-dco-pag         pic 9(01) Binary. ; Line Feed.

;-----------------------------------------------------------------------------

FD ARC-CHE-F                               ; Archivo de CHEQUES
   recording mode is fixed
   type is 'S'.

01 REG-ARC-CHE.
   05 codigo-arc-che     pic 9(06).        ; Cขdigo del Cliente
   05 nrodoc-arc-che     pic X(08).        ; Nฃmero de Documento.
 ; 05 BoFa-arc-che       pic X(01).        ;
   03 serie-arc-che      pic X(08).        ; Serie del Cheque
   03 c-bco-arc-che      pic 9(02).        ; Cขdigo del Banco
   03 fecha-arc-che      pic 9(06).        ; Fecha del Cheque
   03 monto-arc-che      pic 9(06)V99.     ; Monto del Cheque

;-----------------------------------------------------------------------------

FD ARC-CAR-F                               ; Archivo de CARGA CAMION
   recording mode is fixed                 ; CARGAS + Codigo-Vendedor.S
   type is 'S'.

01 REG-ARC-CAR.
   03 c-ve-arc-car       pic 9(02).        ; Cขdigo Vendedor
   03 f-arc-car.                           ; Fecha
      05 dia-arc-car     pic 9(02).
      05 mes-arc-car     pic 9(02).
      05 ano-arc-car     pic 9(02).
   03 kilo-arc-car       pic 9(10).        ; Kilometraje
   03 cini-arc-car       pic 9(06)V99.     ; Caja Inicial
   03 caja-arc-car       pic 9(06)V99.     ; Caja Final
   03 cob-arc-car        pic 9(06)V99.     ; Cobranzas
   03 cre-arc-car        pic 9(06)V99.     ; Crditos
   03 ade-arc-car        pic 9(06)V99.     ; Adelantos
   03 con-arc-car        pic 9(06)V99.     ; Contados
   03 cli-con-car        pic 9(03).        ; Clientes Concretados.
   03 cli-vis-car        pic 9(03).        ; Nro.Visitas.
   03 cli-ate-car        pic 9(03).        ; Clientes Atendidos.
   03 cli-can-car        pic 9(03).        ; Clientes Visitados.
   03 cr-arc-car         pic 9(01) Binary. ; Retorno de Carro.
   03 lf-arc-car         pic 9(01) Binary. ; Line Feed.


;ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท
;บ          Archivos Salida de Envases.                                      บ
;ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ

FD CAB-PRE-F                               ; Archivo de CABECERA PRESTAMOS DE
   recording mode is fixed                 ;            ENVASES EN RUTA
   type is 'S'.

01 REG-CAB-PRE.
   03 codigo-cab-pre     pic 9(06).        ; Cขdigo del Cliente
   03 nro-doc-cab-pre    pic X(08).        ; Nฃmero de Documento
   03 cod-pro-cab-pre    pic 9(03).        ; Cขdigo del Envase
;  03 doc-csg-cab-pre    pic X(08).        ;
   03 can-pre-cab-pre    pic 9(08).        ; Cantidad Prestada
   03 fecha-cab-pre      pic 9(06).        ; Fecha de Emisiขn
   03 vend-cab-pre       pic 9(02).        ; Vendedor
   03 ruta-cab-pre       pic 9(04).        ; Ruta
;  03 evcs-cab-pre       pic 9(01).        ; Envases 0=Normal, 1=Consignaciขn
   03 cr-cab-pre         pic 9(01) Binary. ; Retorno de Carro.
   03 lf-cab-pre         pic 9(01) Binary. ; Line Feed.

;-----------------------------------------------------------------------------

FD DEV-ENV-F                               ; Archivo de DEVOLUCIONES
   recording mode is fixed                 ; DE ENVASES
   type is 'S'.

01 REG-DEV-ENV.
   03 cod-cli-dev        pic 9(06).        ; Cขdigo del Cliente
   03 nro-doc-dev        pic X(08).        ; Nฃmero de Documento
   03 doc-ref-dev        pic X(08).        ; Nฃmero de Documento Referencia
   03 cod-pro-dev        pic 9(03).        ; Cขdigo del Envase
   03 can-pre-dev        pic 9(08).        ; Cantidad Devuelta
   03 fecha-dev          pic X(06).        ; Fecha de la Cobranza
   03 vend-dev           pic 9(02).        ; Vendedor
   03 ruta-dev           pic 9(04).        ; Ruta
   03 cr-dev             pic 9(01) Binary. ;
   03 lf-dev             pic 9(01) Binary. ;

;ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท
;บ          Archivos Salida de Consignaciones                                บ
;ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ

FD CAB-CSG-F                               ; Archivo de CONSIGNACIONES
   recording mode is fixed                 ; Otorgadas.
   type is 'S'.

01 REG-CAB-CSG.
   03 codigo-cab-csg     pic 9(06).        ; Cขdigo del Cliente
   03 nro-doc-cab-csg    pic X(08).        ; Nฃmero de Documento
   03 cod-pro-cab-csg    pic 9(03).        ; Cขdigo del Envase
   03 can-pre-cab-csg    pic 9(08).        ; Cantidad Prestada
   03 fecha-cab-csg      pic 9(06).        ; Fecha de Emisiขn
   03 vend-cab-csg       pic 9(02).        ; Vendedor
   03 ruta-cab-csg       pic 9(04).        ; Ruta
   03 Env-cab-csg        pic 9(01).        ; Con Envases = 1, Sin = 0
   03 cr-cab-csg         pic 9(01) Binary. ; Retorno de Carro
   03 lf-cab-csg         pic 9(01) Binary. ; Line Feed

;-----------------------------------------------------------------------------
FD DEV-CSG-F                               ; Archivo de DEVOLUCIONES
   recording mode is fixed                 ; por Consignacion.
   type is 'S'.

01 REG-DEV-CSG.
   03 cod-cli-csg        pic 9(06).        ; Cขdigo del Cliente
   03 nro-doc-csg        pic X(08).        ; Nฃmero de Documento
   03 doc-ref-csg        pic X(08).        ; Nฃmero de Documento Referencia
   03 cod-pro-csg        pic 9(03).        ; Cขdigo del Envase
   03 can-pre-csg        pic 9(08).        ; Cantidad Devuelta
   03 fecha-csg          pic X(06).        ; Fecha de la Cobranza
   03 vend-csg           pic 9(02).        ; Vendedor
   03 ruta-csg           pic 9(04).        ; Ruta
   03 cr-csg             pic 9(01) Binary. ;
   03 lf-csg             pic 9(01) Binary. ;


;-----------------------------------------------------------------------------

FD DevCsg-F                                ;  Archivo de DEVOLUCIONES
   recording mode is fixed                 ;  DE ENVASES DE CONSIGNACION.
   type is 'S'.

01 REG-DevCsg.
   03 cod-cli-dev-csg    pic 9(06).        ; Cขdigo del Cliente
   03 nro-doc-dev-csg    pic X(08).        ; Nฃmero de Documento
   03 doc-ref-dev-csg    pic X(08).        ; Nฃmero de Documento Referencia
   03 cod-pro-dev-csg    pic 9(03).        ; Cขdigo del Envase
   03 can-pre-dev-csg    pic 9(08).        ; Cantidad Devuelta
   03 fecha-dev-csg      pic X(06).        ; Fecha de la Cobranza
   03 vend-dev-csg       pic 9(02).        ; Vendedor
   03 ruta-dev-csg       pic 9(04).        ; Ruta
   03 cr-dev-csg         pic 9(01) Binary. ;
   03 lf-dev-csg         pic 9(01) Binary. ;
;-----------------------------------------------------------------------------

FD PrEvCsg-F                               ; Archivo de CABECERA PRESTAMOS DE
   recording mode is fixed                 ;            ENVASES EN RUTA
   type is 'S'.

01 REG-PrEvCsg.
   03 codigo-csg-pre     pic 9(06).        ; Cขdigo del Cliente
   03 nro-doc-csg-pre    pic X(08).        ; Nฃmero de Documento
   03 cod-pro-csg-pre    pic 9(03).        ; Cขdigo del Envase
   03 can-pre-csg-pre    pic 9(08).        ; Cantidad Prestada
   03 fecha-csg-pre      pic 9(06).        ; Fecha de Emisiขn
   03 vend-csg-pre       pic 9(02).        ; Vendedor
   03 ruta-csg-pre       pic 9(04).        ; Ruta
   03 cr-csg-pre         pic 9(01) Binary. ; Retorno de Carro.
   03 lf-csg-pre         pic 9(01) Binary. ; Line Feed.


;ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท
;บ          Archivos Salida de Varios en Ruta                                บ
;ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ

FD CAB-VAR-F                               ; Archivo de CABECERA VARIOS EN RUTA
   recording mode is fixed
   type is 'S'.

01 REG-CAB-VAR.
   03 nro-doc-cab-var    pic 9(08).        ; Nฃmero del Documento
   03 nombre-cab-var.                      ; Nombre del Cliente
      05 nom-cab-var-01  pic X(20).
      05 nom-cab-var-02  pic X(15).
   03 motivo-cab-var     pic X(32).        ; Motivo de la Transacciขn
   03 dinero-cab-var     pic 9(06)V99.     ; Dinero Recogido

;-----------------------------------------------------------------------------

FD DET-VAR-F                               ; Archivo de DETALLE VARIOS EN RUTA
   recording mode is fixed
   type is 'S'.

01 REG-DET-VAR.
   03 nro-doc-det-var    pic 9(08).        ; Nฃmero del Documento
   03 cod-pro-det-var    pic 9(03).        ; Cขdigo del Envase
   03 canti-det-var      pic 9(06).        ; Cantidad Recogida


;ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท
;บ          Archivos Salida de  Recarga y Descarga de Camiขn.                บ
;ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ


FD RECARGA-F                               ; Archivo de RECARGA DEL CAMION
   recording mode is fixed
   type is 'S'.

01 REG-RECARGA.
   03 NroDoc-rec         pic X(08).        ; Cขdigo del Producto
   03 CodPrd-rec         pic X(03).        ; Cขdigo del Producto
   03 CntPrd-rec         pic 9(08).        ; Cantidad
   03 Ruta-rec           pic 9(04).        ; Ruta
   03 Fecha-rec          pic 9(06).        ; Fecha
   03 Hora-rec           pic 9(06).        ; Hora
   03 Vend-rec           pic 9(02).        ; Vendedor
   03 Tipo-rec           pic 9(02).        ; 01= Inicial, 02=Recarga
   03 cr-rec             pic 9(01) Binary. ; Retorno de Carro.
   03 lf-rec             pic 9(01) Binary. ; Line Feed.

;-----------------------------------------------------------------------------

FD DESCARGA-F                               ; Archivo de DESCARGAS DEL CAMION
   recording mode is fixed
   type is 'S'.

01 REG-DESCARGA.
   03 NroDoc-des         pic X(08).        ; Cขdigo del Producto
   03 CodPrd-des         pic X(03).        ; Cขdigo del Producto
   03 CntPrd-des         pic 9(08).        ; Cantidad
   03 Ruta-des           pic 9(04).        ; Ruta
   03 Fecha-des          pic 9(06).        ; Fecha
   03 Hora-des           pic 9(06).        ; Hora
   03 cr-des             pic 9(01) Binary. ; Retorno de Carro.
   03 lf-des             pic 9(01) Binary. ; Line Feed.
;-----------------------------------------------------------------------------


FD CENSO-F                                 ; Nombre Archivo CENSO
   recording mode is fixed                 ; CENSO-+Codigo-Vendedor.S
   type is 'S'.                            ;

01 REG-CENSO.
   03 cens-llave.
      05 cens-cliente    pic 9(06).        ; Codigo Cliente
      05 cens-producto   pic 9(08).        ; Codigo Producto
   03 cens-cantidad      pic 9(07).        ; Cantidad registrada
   03 cens-c1            pic 9(01) binary.
   03 cens-c2            pic 9(01) binary.

;-----------------------------------------------------------------------------

FD ENCUESTA-F                              ; Nombre Archivo ENCUESTA
   recording mode is fixed                 ; ENCUES+Codigo-Vendedor.S
   type is 'S'.                            ;

01 REG-ENCUESTA.
   03 encu-llave.
      05 encu-codigo     pic 9(06).        ; Codigo Cliente
      05 encu-producto   pic 9(08).        ; Codigo Producto
   03 encu-precio        pic 9(06).        ; Precio inicial encuesta
   03 encu-c1            pic 9(01) binary.
   03 encu-c2            pic 9(01) binary.

;-----------------------------------------------------------------------------

FD MERCHAND-F                              ; Archivo de MERCADEO
   recording mode is fixed
   type is 'S'.

01 REG-MERCHAND.
   03 CM-COD-CLI         PIC 9(6).
   03 CM-DESCRIP         PIC X(48).
   03 CM-C1              pic 9(01) binary.
   03 CM-C2              pic 9(01) binary.

;-----------------------------------------------------------------------------

FD PUBLICIDAD-F                            ; Resultado de Encuaesta de
   recording mode is fixed                 ; PUBLICIDAD
   type is 'S'.

01 REG-PUBLICIDAD.
   03 CLAVE-PUBLICIDAD.
      05 CP-COD-CLI     PIC 9(6).
      05 CP-COD-ITEM    PIC X(3).
   03 CP-CANTIDAD       PIC 9(6).
   03 CP-C1              pic 9(01) binary.
   03 CP-C2              pic 9(01) binary.

;-----------------------------------------------------------------------------

FD CANJES-F                                ; Archivo de Canjes
   recording mode is fixed                 ;
   type is 'S'.

01 REG-CANJES.
   03 cj-CodCli          pic 9(06).        ;
   03 cj-Nrodoc          pic X(08).        ; Nro. Comprobante de Canje.
   03 cj-CodPrd          pic 9(03).
   03 cj-CntPrd          pic 9(08).
   03 cj-Razon           pic 9(01).
   03 cj-Ruta            pic 9(04).
   03 cj-Fecha           pic 9(06).
   03 cj-cr              pic 9(01) Binary. ; Retorno de Carro.
   03 cj-lf              pic 9(01) Binary. ; Line Feed.

;-----------------------------------------------------------------------------

FD RACIONES-F                              ; Archivo de Raciones.
   recording mode is fixed                 ;
   type is 'S'.

01 REG-RACIONES.
   03 rc-nrodoc          pic X(08).        ; Nro. Ticket de Raciขn.
   03 rc-codpro          pic X(03).
   03 rc-cantidad        pic 9(06).
   03 rc-cr              pic 9(01) Binary. ; Retorno de Carro.
   03 rc-lf              pic 9(01) Binary. ; Line Feed.

;-----------------------------------------------------------------------------

FD ROTURAS-F                               ; Archivo de Roturas.
   recording mode is fixed                 ;
   type is 'S'.

01 REG-ROTURAS.
   03 Rt-nrodoc          pic 9(08).        ; Nro. de Rotura.
   03 Rt-CodPrd          pic 9(03).
   03 Rt-CntPrd          pic 9(06).
   03 Rt-Fecha           pic 9(06).
   03 Rt-Ruta            pic 9(04).
   03 Rt-NroUnd          pic 9(02).
   03 Rt-cr              pic 9(01) Binary. ; Retorno de Carro.
   03 Rt-lf              pic 9(01) Binary. ; Line Feed.

;-----------------------------------------------------------------------------

FD OBSEQUIOS-F                             ; Archivo de Obsequios
   recording mode is fixed                 ;
   type is 'S'.

01 REG-OBSEQUIOS.
   03 Ob-NroDoc          pic 9(08).        ; Nro. de Obsequio.
   03 Ob-CodPrd          pic 9(03).
   03 Ob-CntPrd          pic 9(06).
   03 Ob-Fecha           pic 9(06).
   03 Ob-Ruta            pic 9(04).
   03 Ob-NroUnd          pic 9(02).
   03 Ob-cr              pic 9(01) Binary. ; Retorno de Carro.
   03 Ob-lf              pic 9(01) Binary. ; Line Feed.

;-----------------------------------------------------------------------------

FD GASTOS-F                                ; Archivo de Gastos en Ruta.
   recording mode is fixed                 ;
   type is 'S'.

01 REG-GASTOS.
   03 Gs-Documento       pic X(08).
   03 Gs-CntDin          pic 9(08).
   03 Gs-Concepto        pic 9(32).
   03 Gs-cr              pic 9(01) Binary. ; Retorno de Carro.
   03 Gs-lf              pic 9(01) Binary. ; Line Feed.

                ;ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท;
                ;บ    ARCHIVOS DE SPOOLING PARA IMPRESION     บ;
                ;ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ;

FD PRIN-01-F                               ; Archivo de SPOOL-01 CABECERA FACTURA
   recording mode is fixed
   type is 'P'.

01 REG-PRINT-01.
   03 p-01-codigo        pic 9(06).        ; Cขdigo del Cliente
   03 p-01-factura.                        ; Nฃmero de la Factura X(08)
      05 p-01-fser       pic X(03).
      05 p-01-fcor       pic X(05).
   03 p-01-bofa          pic X(01).        ; Nฃmero de la Factura
   03 p-01-date.                           ; Fecha de la Venta
      05 p-01-dia        pic 9(02).
      05 p-01-mes        pic 9(02).
      05 p-01-ano        pic 9(02).
   03 p-01-time.                           ; Hora de la Venta
      05 p-01-hora       pic 9(02).
      05 p-01-minu       pic 9(02).
      05 p-01-segu       pic 9(02).
   03 p-01-nomb-esta     pic X(32).        ; Nombre del establecimiento
   03 p-01-ruc.                            ; RUC del Cliente
      05 p-01-serie      pic 9(07).
      05 p-01-digito     pic X(01).
   03 p-01-direccion     pic X(32).        ; Direcciขn del Cliente
   03 p-01-vendedor      pic 9(02).        ; Cขdigo del Vendedor
   03 p-01-monto-1       pic 9(06)V99.     ; Monto Neto de la Factura
   03 p-01-monto-2       pic 9(06)V99.     ; IGV de la Factura
   03 p-01-monto-3       pic 9(06)V99.     ; Monto Total de la Factura
   03 p-01-tipo          pic 9(01).         ; 0: credito; 1: contado
   03 p-01-ind-adelanto  pic 9(01).
   03 p-01-monto-adelan  pic 9(06)V99.
   03 p-01-monto-dsc     pic 9(06)V99.
   03 p-01-banco         pic 9(02).
   03 p-01-cheque        pic X(08).
   03 p-01-dias          pic 9(02).

;-----------------------------------------------------------------------------

FD PRIN-02-F                               ; Archivo de SPOOL-02 DETALLE FACTURA
   recording mode is fixed
   type is 'P'.

01 REG-PRINT-02.
   03 p-02-codigo        pic 9(06).        ; Cขdigo del Cliente
   03 p-02-factura       pic X(08).        ; Nฃmero de la Factura
   03 p-02-cod-prod      pic 9(03).        ; Cขdigo del Producto
   03 p-02-bofa          pic X(01).        ;
   03 p-02-des-prod      pic X(15).        ; Descripciขn del Producto
   03 p-02-pre-prod      pic 9(06)V99.     ; Precio Unitario.
   03 p-02-can-prod      pic 9(08).        ; Cantidad del Producto
   03 p-02-par-prod      pic 9(08).        ; Acumulado
   03 p-02-uni-prod      pic 9(02).        ;
   03 p-02-prc-prod      pic 9(08).        ; Precio Modificado de producto.
   03 p-02-dsc-prod      pic 9(08).        ; Descuento de Producto.

;-----------------------------------------------------------------------------

FD PRIN-04-F                               ; Archivo de SPOOL-04 CABECERA
   recording mode is fixed                 ;         RECIBO DE DINERO
   type is 'P'.

01 REG-PRINT-04.
   03 p-04-documento     pic X(08).        ; Nฃmero del Documento
   03 p-04-cod-vende     pic 9(02).        ; Cขdigo Vendedor
   03 p-04-efectivo      pic 9(06)V99.     ; Monto del Efectivo

;-----------------------------------------------------------------------------

FD PRIN-05-F                               ; Archivo de SPOOL-05 DETALLE CHEQUES
   recording mode is fixed                 ;         RECIBO DE DINERO
   type is 'P'.

01 REG-PRINT-05.
   03 p-05-documento     pic X(08).        ; Nฃmero del Documento
   03 p-05-serie         pic X(08).        ; Serie Cheque
   03 p-05-codigo        pic 9(02).        ; Cขdigo del Banco
   03 p-05-monto         pic 9(06)V99.     ; Monto del Cheque

;-----------------------------------------------------------------------------

FD PRIN-06-F                               ; Archivo de SPOOL-06 CABECERA MUESTREO
   recording mode is fixed
   type is 'P'.

01 REG-PRINT-06.
   03 p-06-codigo        pic 9(06).        ; Cขdigo del Cliente
   03 p-06-estado        pic X(15).        ; Estado del Cliente
   03 p-06-factura       pic X(08).        ; Nฃmero de la Factura
   03 p-06-time.                           ; Hora de la Venta
      05 p-06-hora       pic 9(02).
      05 p-06-minu       pic 9(02).
      05 p-06-segu       pic 9(02).
   03 p-06-nomb-esta     pic X(15).        ; Nombre del establecimiento
   03 p-06-direccion     pic X(20).        ; Direcciขn del Cliente

;-----------------------------------------------------------------------------

FD PRIN-07-F                               ; Archivo de SPOOL-07 DETALLE INFORME
   recording mode is fixed
   type is 'P'.

01 REG-PRINT-07.
   03 p-07-codigo        pic 9(06).        ; Cขdigo del Producto
   03 p-07-descri        pic X(16).        ; Descripciขn del Producto
   03 p-07-inicial       pic 9(06).        ; Stock Inicial
   03 p-07-ventas        pic 9(06).        ; Ventas
   03 p-07-final         pic 9(06).        ; Stock Final

;-----------------------------------------------------------------------------

FD PRIN-08-F                               ; Archivo de SPOOL-08
   recording mode is fixed                 ; RECIBO DE PRESTAMO
   type is 'P'.

01 REG-PRINT-08.
   03 p-08-documento     pic X(08).        ; Nฃmero del Documento
   03 p-08-cod-prod      pic 9(03).        ; Codigo Producto
   03 p-08-cod-cli       pic X(06).        ; Codigo de Cliente
   03 p-08-nom-cli       pic X(25).        ; Nombre de Cliente
   03 p-08-dir-cli       pic X(25).        ; Direcciขn del Cliente
   03 p-08-date.                           ; Fecha de la Cobranza
      05 p-08-dia        pic 9(02).
      05 p-08-mes        pic 9(02).
      05 p-08-ano        pic 9(02).
   03 p-08-time.                           ; Hora de la Cobranza
      05 p-08-hora       pic 9(02).
      05 p-08-minu       pic 9(02).
      05 p-08-segu       pic 9(02).
   03 p-08-cnt-prod      pic 9(08).        ; Cantidad producto
   03 p-08-dsc-prod      pic 9(16).        ; Descripcion del Producto.
   03 p-08-und-prod      pic 9(02).        ;

;-----------------------------------------------------------------------------

FD PRIN-09-F                               ; Archivo de SPOOL-09
   recording mode is fixed                 ; RECIBO DE DEVOLUCION
   type is 'P'.

01 REG-PRINT-09.
   03 p-09-doc-pre       pic X(08).        ; Documento de prstamo.
   03 p-09-cod-prod      pic 9(03).        ; Codigo Producto
   03 p-09-documento     pic X(08).        ; Nฃmero del Documento
   03 p-09-cod-cli       pic X(06).        ; Codigo de Cliente
   03 p-09-nom-cli       pic X(25).        ; Nombre de Cliente
   03 p-09-dir-cli       pic X(25).        ; Direcciขn del Cliente
   03 p-09-date.                           ; Fecha de la Cobranza
      05 p-09-dia        pic 9(02).
      05 p-09-mes        pic 9(02).
      05 p-09-ano        pic 9(02).
   03 p-09-time.                           ; Hora de la Cobranza
      05 p-09-hora       pic 9(02).
      05 p-09-minu       pic 9(02).
      05 p-09-segu       pic 9(02).
   03 p-09-cnt-prod      pic 9(08).        ; Cantidad producto
   03 p-09-dsc-prod      pic 9(16).        ; Descripcion del Producto.
   03 p-09-und-prod      pic 9(02).        ; Nro. de Unidades.
   03 p-09-cnt-sld       pic 9(08).        ; Saldo de Envases.

;-----------------------------------------------------------------------------

FD PRIN-10-F                               ; Archivo de SPOOL-10
   recording mode is fixed                 ; RECARGA DE PRODUCTOS (CAMION)
   type is 'P'.

01 REG-PRINT-10.
   03 p-10-documento     pic X(08).        ; Nฃmero del Documento
   03 p-10-cod-prod      pic 9(03).        ; Codigo Producto
   03 p-10-cnt-prod      pic 9(08).        ; Cantidad producto

;-----------------------------------------------------------------------------

FD PRIN-11-F                               ; Archivo de SPOOL-06
   recording mode is fixed                 ; RECIBO DE DESCARGA
   type is 'P'.

01 REG-PRINT-11.
   03 p-11-documento     pic X(08).        ; Nฃmero del Documento
   03 p-11-cod-prod      pic 9(03).        ; Codigo Producto
   03 p-11-date.                           ;
      05 p-11-dia        pic 9(02).
      05 p-11-mes        pic 9(02).
      05 p-11-ano        pic 9(02).
   03 p-11-time.                           ; Hora
      05 p-11-hora       pic 9(02).
      05 p-11-minu       pic 9(02).
      05 p-11-segu       pic 9(02).
   03 p-11-cnt-prod      pic 9(06).        ; Cantidad producto
   03 p-11-dsc-prod      pic 9(16).        ; Descripcion del Producto.
   03 p-11-und-prod      pic 9(02).        ;

;-----------------------------------------------------------------------------

FD PRIN-12-F                               ; Archivo de SPOOL-12
   recording mode is fixed                 ; RECIBO DE COBRANZA
   type is 'P'.

01 REG-PRINT-12.
   03 p-12-documento     pic X(08).        ; Nฃmero del Documento
   03 p-12-cod-cli       pic X(06).        ; Codigo de Cliente
   03 p-12-factura       pic X(09).        ; Factura Inicial.
   03 p-12-monto         pic 9(06)V99.     ; Monto del Cheque
   03 p-12-tipo-pago     pic 9(01).        ; Tipo de Pago.
   03 p-12-tipo-dinero   pic 9(01).        ; Tipo de Dinero
   03 p-12-cheque        pic X(08).        ; Cheque
   03 p-12-bco           pic 9(02).        ; Banco
   03 p-12-nom-cli       pic X(30).        ; Nombre de Cliente
   03 p-12-dir-cli       pic X(30).        ; Direcciขn del Cliente
   03 p-12-date.                           ; Fecha de la Cobranza
      05 p-12-dia        pic 9(02).
      05 p-12-mes        pic 9(02).
      05 p-12-ano        pic 9(02).
   03 p-12-time.                           ; Hora de la Cobranza
      05 p-12-hora       pic 9(02).
      05 p-12-minu       pic 9(02).
      05 p-12-segu       pic 9(02).

;-----------------------------------------------------------------------------

FD PRIN-13-F                               ; Archivo de SPOOL-13
   recording mode is fixed                 ; RECIBO DE CONSIGNACION
   type is 'P'.

01 REG-PRINT-13.
   03 p-13-documento     pic X(08).        ; Nฃmero del Documento
   03 p-13-cod-prod      pic 9(03).        ; Codigo Producto
   03 p-13-cod-cli       pic X(06).        ; Codigo de Cliente
   03 p-13-nom-cli       pic X(25).        ; Nombre de Cliente
   03 p-13-dir-cli       pic X(25).        ; Direcciขn del Cliente
   03 p-13-monto         pic 9(06)V99.     ; Monto del Cheque
   03 p-13-tipo-pago     pic 9(01).        ; Tipo de Pago.
   03 p-13-date.                           ; Fecha de la Cobranza
      05 p-13-dia        pic 9(02).
      05 p-13-mes        pic 9(02).
      05 p-13-ano        pic 9(02).
   03 p-13-time.                           ; Hora de la Cobranza
      05 p-13-hora       pic 9(02).
      05 p-13-minu       pic 9(02).
      05 p-13-segu       pic 9(02).
   03 p-13-cnt-prod      pic 9(06).        ; Cantidad producto
   03 p-13-dsc-prod      pic 9(16).        ; Descripcion del Producto.
   03 p-13-und-prod      pic 9(02).        ;

;-----------------------------------------------------------------------------

FD PRIN-14-F                               ; Archivo de SPOOL-08
   recording mode is fixed                 ; RECIBO DE CANJES
   type is 'P'.

01 REG-PRINT-14.
   03 p-14-documento     pic X(08).        ; Nฃmero del Documento
   03 p-14-cod-prod      pic 9(03).        ; Codigo Producto
   03 p-14-razon         pic 9(01).        ; Razon
   03 p-14-cod-cli       pic X(06).        ; Codigo de Cliente
   03 p-14-nom-cli       pic X(32).        ; Nombre de Cliente
   03 p-14-dir-cli       pic X(32).        ; Direcciขn del Cliente
   03 p-14-date          pic 9(06).        ; Fecha de la Cobranza
   03 p-14-cnt-prod      pic 9(08).        ; Cantidad producto
   03 p-14-dsc-prod      pic 9(16).        ; Descripcion del Producto.
   03 p-14-und-prod      pic 9(02).        ;

;-----------------------------------------------------------------------------
FD PRIN-15-F                               ; Archivo de SPOOL-15
   recording mode is fixed                 ; RECIBO DE DEVOLUCION
   type is 'P'.                            ; DE CONSIGNACION

01 REG-PRINT-15.
   03 p-15-doc-pre       pic X(08).        ; Documento de prstamo.
   03 p-15-cod-prod      pic 9(03).        ; Codigo Producto
   03 p-15-documento     pic X(08).        ; Nฃmero del Documento
   03 p-15-cod-cli       pic X(06).        ; Codigo de Cliente
   03 p-15-nom-cli       pic X(25).        ; Nombre de Cliente
   03 p-15-dir-cli       pic X(25).        ; Direcciขn del Cliente
   03 p-15-date.                           ; Fecha de la Cobranza
      05 p-15-dia        pic 9(02).
      05 p-15-mes        pic 9(02).
      05 p-15-ano        pic 9(02).
   03 p-15-time.                           ; Hora de la Cobranza
      05 p-15-hora       pic 9(02).
      05 p-15-minu       pic 9(02).
      05 p-15-segu       pic 9(02).
   03 p-15-cnt-prod      pic 9(08).        ; Cantidad producto
   03 p-15-dsc-prod      pic 9(16).        ; Descripcion del Producto.
   03 p-15-und-prod      pic 9(02).        ; Descripcion del Producto.

FD PRIN-16-F                               ; Archivo Pagos de Facturas.
   recording mode is fixed
   type is 'P'.

01 REG-PRINT-16.
   05 p-16-codigo        pic 9(06).        ; Cขdigo del Cliente
   05 p-16-nfac          pic X(08).        ; Nฃmero del Documento
   05 p-16-bofa          pic X(01).        ; Boleta o Factura
   03 p-16-monto-che     pic 9(06)V99.     ; Monto de Cheque
   03 p-16-monto-efe     pic 9(06)V99.     ; Monto de Efectivo
   03 p-16-bco           pic 9(02).        ; Banco
   03 p-16-nro-chq       pic X(08).        ; Cheque
   03 p-16-date          pic 9(06).        ; Fecha de la Venta
   03 p-16-hora          pic 9(06).        ; Hora de la Venta
;-----------------------------------------------------------------------------

FD PRIN-17-F                               ; Archivo de SPOOL-17
   recording mode is fixed                 ; Adelantos de Envases.
   type is 'P'.

01 REG-PRINT-17.
   03 p-17-documento     pic X(08).        ; Nฃmero del Documento
   03 p-17-cod-prod      pic 9(03).        ; Codigo Producto
;  03 p-17-doc-ref       pic X(08).        ; Documento de prstamo.
   03 p-17-cod-cli       pic X(06).        ; Codigo de Cliente
   03 p-17-nom-cli       pic X(25).        ; Nombre de Cliente
   03 p-17-dir-cli       pic X(25).        ; Direcciขn del Cliente
   03 p-17-date          pic 9(06).        ; Fecha de la Cobranza
   03 p-17-cnt-prod      pic 9(08).        ; Cantidad producto
   03 p-17-dsc-prod      pic 9(16).        ; Descripcion del Producto.
   03 p-17-und-prod      pic 9(02).        ; Descripcion del Producto.

;-----------------------------------------------------------------------------


FD PRINT-18-F                              ; Archivo de SPOOL-17
   recording mode is fixed                 ; Recibo de adelanto de
   type is 'P'.                            ; envases de Consignaciขn.

01 REG-PRINT-18.
   03 p-18-cod-cli       pic X(06).        ; Codigo de Cliente
   03 p-18-documento     pic X(08).        ; Nฃmero del Documento
   03 p-18-cod-prod      pic 9(03).        ; Codigo Producto
   03 p-18-date          pic 9(06).        ; Fecha de la Cobranza
   03 p-18-cnt-prod      pic 9(08).        ; Cantidad producto
   03 p-18-und-prod      pic 9(02).        ; Descripcion del Producto.

;-----------------------------------------------------------------------------

FD PRIN-19-F                               ; Archivo de SPOOL-09
   recording mode is fixed                 ; RECIBO DE DEVOLUCION
   type is 'P'.

01 REG-PRINT-19.
   03 p-19-doc-pre       pic X(08).        ; Documento de prstamo.
   03 p-19-cod-prod      pic 9(03).        ; Codigo Producto
   03 p-19-documento     pic X(08).        ; Nฃmero del Documento
   03 p-19-cod-cli       pic X(06).        ; Codigo de Cliente
   03 p-19-nom-cli       pic X(25).        ; Nombre de Cliente
   03 p-19-dir-cli       pic X(25).        ; Direcciขn del Cliente
   03 p-19-date.                           ; Fecha de la Cobranza
      05 p-19-dia        pic 9(02).
      05 p-19-mes        pic 9(02).
      05 p-19-ano        pic 9(02).
   03 p-19-time.                           ; Hora de la Cobranza
      05 p-19-hora       pic 9(02).
      05 p-19-minu       pic 9(02).
      05 p-19-segu       pic 9(02).
   03 p-19-cnt-prod      pic 9(08).        ; Cantidad producto
   03 p-19-dsc-prod      pic 9(16).        ; Descripcion del Producto.
   03 p-19-und-prod      pic 9(02).        ; Descripcion del Producto.

;-----------------------------------------------------------------------------

                     ;ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท;
                     ;บ    ARCHIVO DE COMUNICACIONES    บ;
                     ;ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ;

FD FILELIST-F
   recording mode is fixed                 ; Lista de archivos a enviar
   type is 'V'.

01 HOST-PARAMS-R.                          ; Primer registro en archivo.
   03 TIPO-HOST-1        pic  X(03).       ; Tipo Host:'PTC','PC ','TDS'.
   03 VELOCIDAD-1        pic  9(02).       ; Velocid:'03','12','96'.
   03 CODI-VEND-1        pic  9(04).       ; Codigo Vendedor.
   03 TELEFONO.
      05 NUM-AREA        pic  X(03).       ; Numero Area
      05 NUM-TELEFONO    pic  X(07).       ; Numero Telefono
   03 SUBMIT-FILE        pic  X(08).       ; Nombre de SUBMIT.
   03 IDENTIF-FILE       pic  X(08).
   03 COMM-MODE          pic  9(01).       ; Tipo comunicacion   - 0 Automatica
   03 FILLER             pic  X(08).       ; 1 Directa - 2 Modem - 3  Polling

FD PROGRAMA-F                              ; Archivo para Efectuar
   recording mode is FREE                  ; las Llamadas a los
   type is program.                        ; Distintos Programas

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                          WORKING STORAGE SECTION                           ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
WORKING-STORAGE section.

01 varios-1.
   03 nAtencion          pic 9(01).
   03 carga-codigo       pic 9(02).
   03 campo1             pic 9(03).
   03 campo2             pic X(13).
   03 campo3             pic 9(06).
   03 total-inve         pic 9(06).
   03 flag-hora          pic 9(01).
   03 prende-apaga       pic 9(01) binary.
   03 proc-ws            pic X(16).
   03 cod-ruta1-ws       pic 9(04).
   03 cod-ruta2-ws       pic 9(04).
   03 cod-ruta3-ws       pic 9(04).
   03 cod-ruta4-ws       pic 9(04).
   03 nCodVd             pic 9(02).
   03 hrs                pic 9(02) binary.
   03 rr                 pic 9(02) binary.
   03 caja-ini-ws        pic 9(06)V99.
   03 dinero-saca-ws     pic 9(06)V99.         ; Entregas de Dinero.
   03 busca-pro-ws       pic 9(03).
   03 carga-ok           pic 9(01) binary.
   03 nRuta              pic 9(04).
   03 ws-cod-folio       pic 9(02).
   03 facini             pic X(08).
   03 bolini             pic X(08).

01 varios-2.
   03 sw-graba-vta       pic 9(01) binary.
   03 cCodPrd            pic 9(03).      ; Se utiliza en Devoluciขn.
   03 d-producto-ws      pic X(16).
   03 can-producto-ws    pic 9(08) Binary.
   03 ws-02-factura      pic X(08).
   03 cSerieBanco        pic X(08).
   03 nBanco             pic 9(02).
   03 CAMPO14            pic 9(06)V99.
   03 CAMPO15            pic 9(06)V99.
   03 CAMPO16            pic 9(06)V99.
   03 CAMPO17            pic 9(06)V99.
   03 CAMPO18            pic 9(06)V99.

01 varios-03.
   03 CAMPO50            pic 9(02).
   03 CAMPO51            pic 9(08).
   03 CAMPO52            pic 9(06)V99.
   03 CAMPO53            pic X(08).
   03 CAMPO54            pic 9(02).
   03 CAMPO55            pic 9(06)V99.
   03 CAMPO56            pic 9(06)V99.
   03 cDirEve.
      05 cDircli            pic X(32).
      05 cDircli1           pic X(03).
   03 cNomEve.
      05 cNomcli            pic X(32).
      05 cNomcli1           pic X(03).


01 datos-03.
   03 nombre-propie-ws   pic X(35).
   03 nombre-estable-ws  pic X(35).
   03 dire-estable-ws    pic X(35).
   03 tele-estable-ws    pic 9(06).

01 datos-cliente.
   03 nCodCli.
      05 nCodCli01       pic 9(02).
      05 nCodCliEv       pic 9(02).
      05 nCodCli02       pic 9(02).

   03 ruc-cliente-ruta.
      05 ruc-cliente-ws  pic 9(07).
      05 digito-ws       pic X(01).

01 Ruc.
   03 cli-rut-ws.
      05 cli-rut1-ws     pic 9(07).
      05 cli-dig-ws      pic X(01).
   03 rest-aux.
      05 resto-aux1      pic 9(02) binary.
      05 resto-aux       pic 9(02) binary.
   03 ws-resto.
      05 ws-resto1       pic 9(03) binary.
   03 ws-dig             pic X(01).
   03 ws-con             pic 9(02) binary.
   03 ws-tbl             pic 9(02) binary.
   03 ws-suma            pic 9(03) binary.
   03 ws-num-aux         pic 9(02) binary.

01 varios-5.
   03 var1-ws            pic 9(06)V99 binary.
   03 var2-ws            pic 9(06)V99 binary.
   03 var3-ws            pic 9(06)V99 binary.
   03 var5-ws            pic 9(06)V99 binary.
   03 saldo-ws           pic 9(06)V99 binary.
   03 var6-ws            pic X(07).
   03 total-nota-ws      pic 9(06)V99 binary.
   03 cancela-ws         pic 9(06)V99 binary.
   03 p-producto-ws      pic 9(06)V99.
   03 swich-venta        pic 9(01) binary.
   03 nFlete             pic 9(08) binary.           ;
   03 nPrcPrd            pic 9(06)V99 binary.
   03 nPrecio            pic 9(06)V99 binary.
   03 nPrecioGral        pic 9(06)V99 binary.
   03 nIgv               pic 9(06)V99 binary.
   03 nAcuIgv            pic 9(06)V99 binary.
   03 nAcuIgvAux         pic 9(06)V99 binary.
   03 nAcuIgvAuxC        pic 9(06)V99 binary.
   03 nAcumulado         pic 9(06)V99 binary.
   03 nAcumuladoc        pic 9(06)V99 binary.
   03 nAcumuladot        pic 9(06)V99 binary.
   03 nAcumDscPrd        pic 9(06)V99 binary.
   03 nDscPrd            pic 9(06)V99 binary.
   03 nAcumDsc           pic 9(06)V99 binary.
   03 nAcumAux           pic 9(06)V99 binary.
   03 nAcumAuxC          pic 9(06)V99 binary.
   03 nResto             pic 9(06)V99 binary.
   03 nResto1            pic 9(06)V99 binary.
   03 nEntero            pic 9(06)V99 binary.
   03 nNumero1           pic 9(06)V99 binary.
   03 nNumero2           pic 9(06)V99 binary.
   03 nCantidad          pic 9(06)V99 binary.
   03 nUnidades          pic 9(06)V99 binary.
   03 nSuma              pic 9(06)V99 binary.
   03 nResta             pic 9(06)V99 binary.
   03 nNroUnd            pic 9(02).
   03 nDescuento         pic 9(08) binary.
   03 cComprobante       pic X(18).
   03 nTipNeg            pic 9(02).
   03 nMontoChq          pic 9(08) binary.
   03 nMontoEfe          pic 9(08) binary.
   03 lFlgFcCs           pic 9(01) binary.     ; Factura = 0 , Csg = 1
   03 lFlgIvCs           pic 9(01) binary.     ; Para facturar Consignaciขn.
   03 lFlgEvCs           pic 9(01) binary.     ;
   03 lFlgPed            pic 9(01) binary.     ;
   03 lFlgSinEv          pic 9(01) binary.     ;
   03 w-01-factura.                        ; Nฃmero de la Factura X(08)
      05 w-01-fser       pic X(02).
      05 w-01-fcor       pic X(06).

01 aDias                   REDEFINES.
   03 nDiasMes           pic 9(02)     OCCURS 12 TIMES.
   03 nDiaIndice         pic 9(02) binary.

01 rut-1                 redefines.
   03 ws-num             pic 9(01) occurs 8 times.

01 fecha-ws     packed.
   03 ano-ws             pic 9(02) packed.
   03 mes-ws             pic 9(02) packed.
   03 dia-ws             pic 9(02) packed.
   03 dow-ws             pic 9(02) packed.
   03 hrs-ws             pic 9(02) packed.
   03 minu-ws            pic 9(02) packed.
   03 sec-ws             pic 9(02) packed.
   03 xxxxx-ws           pic 9(02) packed.
   03 yyyyy-ws           pic 9(02) packed.

01 nFecha.
   03 dia-normal-ws      pic 9(02).
   03 mes-normal-ws      pic 9(02).
   03 ano-normal-ws      pic 9(02).

01 nro-dia-semana.
   03 dow-normal-ws      pic 9(02).

01 nHora.
   03 hrs-normal-ws      pic 9(02).
   03 min-normal-ws      pic 9(02).
   03 seg-normal-ws      pic 9(02).

01 ws-file-name.
   03 nombre-fn          pic X(06).
   03 codigo-fn          pic X(02).

01 punteros.
   03 punt-mp            pic 9(01) binary.
   03 punt-car           pic 9(01) binary.
   03 punt-ped           pic 9(01) binary.
   03 punt-comu          pic 9(01) binary.
   03 punt-ruta          pic 9(01) binary.
   03 punt-pago          pic 9(01) binary.
   03 punt-ingreso       pic 9(01) binary.
   03 sw-error           pic 9(01) binary.
   03 punt-pres          pic 9(01) binary.
   03 punt-varios        pic 9(01) binary.
   03 punt-censo         pic 9(01) binary.
   03 sw-censo           pic 9(01) binary.
   03 punt-cancela       pic 9(01) binary.
   03 punt-cobra         pic 9(01) binary.
   03 punt-tipo          pic 9(01) binary.     ;Contado o Crdito
   03 punt-canjes        pic 9(01) binary.
   03 punt-bofa          pic 9(01) binary.
   03 punt-fc            pic 9(01) binary.
   03 punt-consulta      pic 9(01) binary.
   03 menu-ruta          pic 9(01) binary.
   03 punt-dia-vis       pic 9(01) binary.

01 reg-auxiliar-01.
   03 ws-01-fact         pic X(09).             ; Cobranzas
   03 ws-01-monto        pic 9(06)V99 binary.
   03 ws-02-monto        pic 9(06)V99 binary.
   03 ws-01-tipo         pic 9(01).

01 var-pantalla.
   03 resp-pantalla      pic X(64).
   03 resp-pantalla1     pic X(64).

01 Clave_envases.
   03 Documento          pic X(08).
   03 Producto           pic 9(03).

01 Clave_consignacion.
   03 Cliente            pic X(06).
   03 ProductoCsg        pic 9(03).

01 var-cobranza.
   03 fecha-emisi-ws.
      05 dia-emisi-ws    pic 9(02).
      05 mes-emisi-ws    pic 9(02).
      05 ano-emisi-ws    pic 9(02).
   03 fecha-venci-ws.
      05 dia-venci-ws    pic 9(02).
      05 mes-venci-ws    pic 9(02).
      05 ano-venci-ws    pic 9(02).
   03 monto-adeuda-ws    pic 9(06)V99 binary.
   03 pagoa-cuenta-ws    pic 9(06)V99 binary.
   03 tecla-beep         pic 9(01) Binary.
   03 tecla-tono         pic 9(02) binary.
   03 ws-prende-ptc.
      05 prende-ws-1     pic 9(01).
      05 prende-ws-2     pic X(06).
   03 nDias              pic 9(02).
   03 motivo-trans-ws    pic X(32).
   03 amortiza-ws        pic 9(06)V99 binary.
   03 nSaldo             pic 9(06)V99 binary.
   03 nRelEnv1           pic 9(03).
   03 nRelEnv2           pic 9(03).

01 Consulta_Saldos.
   03 nSldTot            pic 9(06)V99 binary.
   03 nSldEnv            pic 9(06)V99 binary.
   03 nSldCsg            pic 9(06)V99 binary.


01 estadistica-ws.
   03 busca-factura.
      05 b1-fact         pic 9(06).
      05 b2-fact         pic X(08).
   03 cant-muestreo-ws   pic 9(02).
   03 fin-ws             pic 9(02) binary.
   03 tot1-ws            pic 9(06) binary.
   03 tot2-ws            pic 9(06) binary.
   03 tot3-ws            pic 9(06) binary.
   03 tot4-ws            pic 9(06) binary.
   03 suma1-ws           pic 9(06)V99 binary.
   03 suma2-ws           pic 9(06)V99 binary.
   03 nNumRed            pic 9(06)V99 binary.
   03 numero.
         05 numero-1.
            07 numero-x  pic 9(06).
            07 numero-y  pic 9(01).
         05 numero-a     pic 9(01).
         05 numero-b     pic 9(01).
         05 numero-c     pic 9(01).
   03  num1-conf         pic 9(01).
   03  num2-conf         pic 9(01).
   03 paso               pic 9(01).


01 var-censos.
   03 busca-censo.
      05 busca-censo1    pic 9(06).
      05 busca-censo2    pic 9(08).
   03 cant-censo         pic 9(07) binary.
   03 cant-encuesta      pic 9(06).

01 var-impresion.
   03 nom-vende-ws       pic X(16).
   03 linea-ws.
      05 linea1-ws       pic X(21).
      05 linea2-ws       pic X(21).

01 varios-ws.                              ; Aumentar esta seccion
   03 cNroDoc            pic X(08).        ; Nro. de Documento Alfanumrico.
   03 cDocEnv            pic X(08).        ; Nro. de Documento Alfanumrico.
   03 nCntPrd            pic 9(08) Binary. ; Cantidad de Producto.
   03 nCanPrd            pic 9(08) Binary. ; Cantidad de Producto.
   03 nVentas            pic 9(08) Binary. ; Cantidad de Ventas Contado
   03 nStock             pic 9(08) Binary. ; Cantidad de Ventas Contado
   03 nRazon             pic 9(01).        ; Concepto de Canje.
   03 nCntDin            pic 9(08) Binary. ;
   03 cConcepto          pic X(32).        ;
   03 cFactura.
      05 nNroDoc.
         07 nNroSer         pic 9(03).     ; Nro. de Documento en Numrico.
         07 nNroFac         pic 9(05).     ; 
      05 cBoFa              pic X(01).     ;
   03 nDocumento.
      05 nSerie             pic 9(02).     ;
      05 nCorrelativo       pic 9(06).     ;
   03 nProcAct           pic 9(01) Binary.
   03 nPanCons           pic 9(01) Binary.
   03 nPanCli            pic 9(01) Binary.
   03 nDiaRuta           pic 9(01) Binary.
   03 cDias_Visita.
      05 dia-1           pic X(01).
      05 dia-2           pic X(01).
      05 dia-3           pic X(01).
      05 dia-4           pic X(01).
      05 dia-5           pic X(01).
      05 dia-6           pic X(01).

01 var-punt-archivo.
    03 LARGO-ARCHIVO    PIC 9(6) BINARY.
    03 LARGO-REGISTRO   PIC 9(6) BINARY.
    03 PUNT-INICIO      PIC 9(6) BINARY.
    03 PUNT-FIN         PIC 9(6) BINARY.
    03 NRO-REG          PIC 9(3) BINARY.

01 memoria.
   03 memory-flag        pic 9(02) binary.
   03 display-save       pic X(132).
   03 ws-mem-avail       pic 9(08) binary.

01 WS-SPEC-CHARS.
   03 WS-CHAR-DEF        PIC 9(16) BINARY.


;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                              KEYBOARD SECTION                              ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
KEYBOARD SECTION.
kd  std-710  value is

; Unshifted

        off   UAR     ER    DAR    KEND
        KMENU KEXIT   LIGHT KELIM  KNEXT
        KINS  ER      KMOD  ADJ    KBEEP
        KMEM  ER      ER    KQUIT  KNOTA
        ER    ER      KVER  KVEND  ER
      shift   YES     KFIND ER     CLR
        '7'   '8'     '9'   BAR
        '4'   '5'     '6'   FAR
        '1'   '2'     '3'   ENTER
        NO    '0'     '.'

; Shifted

       off     'A'     'B'     'C'     KEND
       'D'     'E'     'F'     'G'     'H'
       'I'     'J'     'K'     'L'     'M'
       'N'     'O'     'P'     'Q'     'R'
       'S'     'T'     'U'     'V'     'W'
      shift    'X'     'Y'     'Z'     CLR
       '7'     '8'     '9'     BAR
       '4'     '5'     '6'     FAR
       '1'     '2'     '3'    ENTER
       '-'     '0'     ' '.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                              SCREEN SECTION                                ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
SCREEN SECTION.

SD NUMERO-sd.
03 var1-ws
   mask   56, "99999999".

SD COD-RAMO-SD.
03 nTipNeg
   clear
   edit
   prompt 01, "Tipo Negocio:  "
   mask   15, "99".

SD DESC-RAMO-SD.
03 ra-descripcion
   edit
   mask 17, "XXXXXXXXXXXXXXXX".

SD DESC-BANCO-SD.
03 bc-descripcion
   edit
   mask 49, "XXXXXXXXXXXXXXXX".


SD CARGA-CODIGO-SD.
03 nCodVd
   prompt  1, 'Codigo Vendedor:'
   mask   54, '99'.

SD PRENDE-PTC-SD.
03 ws-prende-ptc
  ;auto-shift
   edit
   alphanumeric
   mask 53, 'BBBBBBB'.

SD PROC-SD.
03 proc-ws
   prompt 17, " Proceso Activo "
   mask   33, "XXXXXXXXXXXXXXXX".

SD RUT-SD.
03 cli-rut-ws
   edit
  ;min 1
   prompt 49, "Ruc :           "
   mask   56, "99999999".

SD DIGV-SD.
03 cli-dig-ws
   min 1
   mask 64, 'X'.

SD COD-VENDE-SD.
03 nCodVd
   clear
;  edit
   prompt  1, 'Cod.Vendedor:   '
   mask   15, '99'.

SD FECHA-HOY-SD.
03 dia-normal-ws
   edit
   prompt 33, 'Fecha :   /  /  '
   mask   41, '99'.
03 mes-normal-ws
   mask   44, '99'.
03 ano-normal-ws
   mask   47, '99'.

SD HORA-HOY-SD.
03 hrs-normal-ws
   edit
   prompt 49, 'Hora  :   :  :  '
   mask   57, '99'.
03 min-normal-ws
   mask   60, '99'.
03 seg-normal-ws
   mask   63, '99'.


SD RUTA-HOY-SD.
03 cod-ruta1-ws
   edit
   prompt 49, 'Ruta:     -     '
   mask   55, '9999'.
03 cod-ruta2-ws
   edit
   mask   61, '9999'.

SD RUTA1-HOY-SD.
03 cod-ruta3-ws
   edit
   prompt 49, 'Ruta:     -     '
   mask   55, '9999'.
03 cod-ruta4-ws
   edit
   mask   61, '9999'.

SD NOM-VENDEDOR-SD.
03 nom-vende-ws
   edit
   auto-shift
   mask 17, 'XXXXXXXXXXXXXXXX'.

SD CAJA-INI-SD.
03 caja-ini-ws
   edit
   clear
   min 1
   prompt  1, '****************Ing.Caja Inicial S/.:           ****************'
   mask   39, 'ZZZZZ9.99'.

SD CANT-PROD-SD.
03 nCntPrd
   edit
   prompt 49, 'Cantid:         '
   mask   56, 'ZZZZZ9.99'.

SD TOTAL-VENTA-SD.
03 var1-ws
   clear
   edit
   prompt  1, 'Venta:          I.G.V.:          Total Venta :  S/.:            '
   mask    8, 'ZZZZZ9.99'.
03 var3-ws
   edit
   mask   24, 'ZZZZZ9.99'.
03 var5-ws
   edit
   mask   53, 'ZZZ,ZZ9.99'.

SD TOTAL-VENTA1-SD.
03 nAcumuladoC
   clear
   edit
   prompt  1, 'SubTot:         Dscto.:         IGV.:            S/.:            '
   mask    8, 'ZZZZZ9.99'.
03 nAcumDscPrd
   edit
   mask   24, 'ZZZZZ9.99'.
03 var3-ws
   edit
   mask   39, 'ZZZ,ZZ9.99'.
03 var5-ws
   edit
   mask   55, 'ZZZ,ZZ9.99'.

SD FCDESCUENTO-SD.
03 nDescuento
   edit
   prompt 33, 'Dscto:          '
   mask   40, 'ZZZZZ9.99'.


SD CANCELA-SD.
03 cancela-ws
   clear
   edit
   prompt 17, 'Monto a Cancelar S/.:'
   mask   38, 'ZZZ,ZZ9.99'.

SD CODIGO-CLIENTE-SD.
03 nCodCli
   clear
   edit
   prompt  1, 'Cod.Cli.:                                       ->              '
   mask   11, '999999'.
03 cNomcli
   mask   17, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

SD DIRE-CLIENTE-SD.
03 cDirCli
   clear
   auto-shift
   prompt  1, '   Direccion:   '
   mask   17, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.


SD CODIGO-PRODUCTO-SD.
03 cCodPrd
   clear
   edit
   prompt  1, 'Cod.Prod.:      '
   mask   14, '999'.

SD P-PRODUCTOS-SD.
03 cCodPrd
   clear
   edit
   prompt  1, 'Cod.Prod.:                      Precio:         '
   mask   14, '999'.
03 d-producto-ws
   edit
   mask   17, 'XXXXXXXXXXXXXXXX'.
03 p-producto-ws
   edit
   mask   40, 'ZZZZZ9.99'.

SD CAN-PRODUCTO-SD.
03 nCntPrd
   edit
   prompt 49, 'Cantid:         '
   mask   56, 'ZZZZZ9.99'.

SD FcCambiaPrecio-SD.
03 p-producto-ws
   edit
   prompt 1, 3, 'Precio:         '
   mask   8, 3, 'ZZZZZ9.99'.

SD FcDscPrd-SD.
03 nDscPrd
   edit
   prompt 1, 3, 'Dscto.:         '
   mask   8, 3, 'ZZZZZ9.99'.

SD FcFlete-SD.
03 nFlete
   edit
;  prompt 1, 4, 'Flete :         '
   mask   8, 3, '999999.99'.

SD FcAnula-SD.
03 nNroDoc
   clear
   edit
   prompt  1, 'Numero:         '
   mask    8, 'XXXXXXXX'.


SD SERIE-BCO-SD.
03 cSerieBanco
   clear
   edit
   auto-shift
   prompt  1, 'Datos del ChequeN.Cheq.:        '
   mask   25, 'XXXXXXXX'.

SD CODIGO-BANCO-SD.
03 nBanco
   edit
   prompt 33, 'Codigo Banco:   '
   mask   47, 'Z9'.

SD MONTO-CHEQ-SD.
03 nMontoChq
   edit
   prompt 49, 'S/. :           '
   mask   55, 'ZZZ,ZZ9.99'.

SD NOMBRE-PROPIE-SD.
03 nombre-propie-ws
   clear
   auto-shift
   min 1
   prompt  1, 'Nombre Propiet.:'
   mask   17, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

SD NOMBRE-ESTABLE-SD.
03 nombre-estable-ws
   clear
   auto-shift
   prompt  1, 'Nombre Estable.:'
   mask   17, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

SD DIRE-ESTABLE-SD.
03 dire-estable-ws
   clear
   auto-shift
   min 1
   prompt  1, 'Direccion Estab:'
   mask   17, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.


SD NOMBRE-EVENTUAL-SD.
03 cNomEve
   clear
   auto-shift
   min 1
   prompt  1, 'Nombre Eventual:'
   mask   17, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

SD DIRE-EVENTUAL-SD.
03 cDirEve
   clear
   auto-shift
   prompt  1, 'Direcc.Eventual:'
   mask   17, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.


SD TELE-ESTABLE-SD.
03 tele-estable-ws
   clear
   edit
   prompt 17, 'Telefono Estab.:'
   mask   38, 'ZZZZZ9'.

SD COBRANZA-SD.
03 ws-01-fact
   clear
   edit
   prompt  1, 'Factur:         Emision:  /  /  Vencimi:  /  /  Saldo:          '
   mask    8, 'XXXXXXXXX'.
03 dia-emisi-ws
   edit
   mask   25, '99'.
03 mes-emisi-ws
   edit
   mask   28, '99'.
03 ano-emisi-ws
   edit
   mask   31, '99'.
03 dia-venci-ws
   edit
   mask   41, '99'.
03 mes-venci-ws
   edit
   mask   44, '99'.
03 ano-venci-ws
   edit
   mask   47, '99'.
03 monto-adeuda-ws
   edit
   mask   55, 'ZZZ,ZZ9.99'.

SD CbMuestraSaldo-SD.
03 nSaldo
   edit
   prompt  1, 4, 'Saldo:          '
   mask   54, 'ZZZ,ZZ9.99'.

SD FcMuestraSaldo-SD.
03 nSaldo
   edit
   prompt  1, 4, 'Saldo:          '
   mask   54, 'ZZZ,ZZ9.99'.


SD COBRANZA-SALDO-SD.
03 ws-01-monto
   edit
   mask   55, 'ZZZ,ZZ9.99'.


SD MOTIVO-SD.
03 ncomp-descripcion
   mask 33, 'XXXXXXXXXXXXXXXX'.

SD DIAS-VENCI-SD.
03 nDias
   edit
   clear
   min 1
   prompt  1, ' Vencimiento :         Dias      Pago a Cuenta: S/. :           '
   mask   21, '99'.

SD PAGOA-CUENTA-SD.
03 pagoa-cuenta-ws
   edit
   mask   55, 'ZZZ,ZZ9.99'.

SD MONTO-ADEUDA-SD.
03 monto-adeuda-ws
   clear
   edit
   prompt 17, 'Monto Adeudado :S/.:            '
   mask   39, 'ZZZ,ZZ9.99'.

SD SOLO-HAY-SD.
03 can-producto-ws
   edit      ;'Stock :         '
   mask  56, 'ZZZZZ9.99'.

SD ENVASES-UNO-SD.
03 cNroDoc
   clear
   prompt 1, 'Numero Documento                Fecha de Emision      /  /      '
   mask  21, 'XXXXXXXX'.
03 dia-emisi-ws
   mask   53, '99'.
03 mes-emisi-ws
   mask   56, '99'.
03 ano-emisi-ws
   mask   59, '99'.


SD CS-DOC-SHOW-SD.
03 cNroDoc
   prompt 17, "Referen:        "
   mask  25, 'XXXXXXXX'.

03 d-producto-ws
   mask  33, 'XXXXXXXXXXXXXXXX'.

03 nStock
   edit
   prompt 49, 'Saldo :'
   mask  56, 'ZZZZZ9.99'.


SD PE-DOC-SHOW-SD.
03 cNroDoc
   prompt  1, "Pedido:        "
   mask   8, 'XXXXXXXX'.
03 dia-emisi-ws
   prompt 17, 'Fecha :   /  /  '
   mask   25, '99'.
03 mes-emisi-ws
   mask   28, '99'.
03 ano-emisi-ws
   mask   31, '99'.
03 d-producto-ws
   mask  33, 'XXXXXXXXXXXXXXXX'.
03 nStock
   edit
   prompt 49, 'Cantid:'
   mask  56, 'ZZZZZ9.99'.



SD DV-SHOW-DOC.
03 cNroDoc
   prompt 49, "Referen:        "
   mask  57, 'XXXXXXXX'.


SD ENVASES-DOS-SD.
03 cCodPrd
   clear
   prompt 1, 'Cod.Prod.:                      Saldo :         '
   mask  13, '999'.
03 d-producto-ws
   mask  17, 'XXXXXXXXXXXXXXXX'.

SD ENVASES-TRES-SD.
03 nStock
   edit
   mask  40, 'ZZZZZ9.99'.

SD ENVASES-ADEL-SD.
03 nSaldo
   edit
   mask  40, 'ZZZZZ9.99'.

SD CANT-PRODUCTO-SD.
03 nCntPrd
   edit
   prompt 49, 'Recib:          '
   mask   56, 'ZZZZZ9.99'.

SD COD-CLIEN-SD.
03 nCodCli
   clear
   prompt  1, 'Cod.Clie.:      '
   mask   11, '999999'.

SD NRO-DOCUMEN-SD.
03 nNroDoc
   edit
   prompt 49, 'Nro.Doc:        '
   mask   57, 'XXXXXXXX'.


SD NRO-DOC-SER-SD.
03 nNroSer
   auto-record
   edit
   prompt 17, 'NroDoc:   -     '
   mask   24, 'XXX'.

SD NRO-DOC-NRO-SD.
03 nNroFac
   edit
   mask   28, 'XXXXX'.

SD NRO-DOCCSG-SD.
03 cNroDoc
   edit
   prompt 49, 'Doc.Csg:        '
   mask   57, 'XXXXXXXX'.

SD FECHA-EMISI-SD.
03 dia-emisi-ws
   prompt 33, 'Fecha :   /  /  '
   mask   41, '99'.
03 mes-emisi-ws
   mask   44, '99'.
03 ano-emisi-ws
   mask   47, '99'.

SD DESC-DINE1-SD.
03 nCntDin
   edit
   clear
   prompt 17, 'Entrega EfectivoS/. :           '
   mask   38, 'ZZZ,ZZ9.99'.

SD NUM-DOCUM-SD.
03 cNroDoc
   clear
   prompt  1, 'Nro.Doc:        '
   mask   09, '99999999'.

SD ESTABLE-NOM-SD.
03 nombre-estable-ws
   auto-shift
   min 1
   prompt 17, 'Nombre :        '
   mask   26, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

SD MOTIVO-TRANS-SD.
03 motivo-trans-ws
   auto-shift
   min 1
   prompt 17, ' Ingrese Motivo '
   mask   33, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

SD AMORTIZA-SD.
03 amortiza-ws
   edit
   clear
   prompt 17, 'Cant. AmortizadaS/. :           '
   mask   38, 'ZZZ,ZZ9.99'.

SD CANT-MUESTREO-SD.
03 cant-muestreo-ws
   clear
   edit
   prompt  1, '****************Ingrese Cantidad                ****************'
   mask   40, 'Z9'.

SD ENVASE-CENSO-SD.
03 embo-empresa
   clear
   edit
   mask  1, 'XXXXXXXXXXXXXXXX'.
03 prce-descripcion
   edit
   mask 17, 'XXXXXXXXXXXXXXXX'.

SD CANTI-CENSO-SD.
03 cant-censo
   edit
   prompt 49, 'Cantidad:       '
   mask   58, 'ZZZZZZ9'.

SD TOT-MODI-CENSO-SD.
03 cens-cantidad
   edit
   prompt 33, ' Total  :       '
   mask   42, 'ZZZZZZ9'.

SD CANTI-ENCUESTA-SD.
03 cant-encuesta
   edit
   prompt 49, 'Precio $:       '
   mask   59, 'ZZZZZ9'.

SD MODI-CENSO-SD.
03 cant-censo
   edit
   prompt 49, 'Cantidad:       '
   mask   58, '9999999'.

sd txt-merchand-sd.
03 CM-DESCRIP
   clear
   edit
   min 1
   auto-shift
   prompt 1 1 'C. Merchandising'
   mask   1 2 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

sd publicid-sd.
03 IP-DESCRIP
   clear
   edit
   mask 1 1 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

sd CANT-PUBLICIDAD-SD.
03 CP-CANTIDAD
   edit
   prompt 1 4 'Cant:'
   mask   7 4 'ZZZZZ9'.

sd REC-DINERO-SD.
03 caja-ini-ws         ;variable de nombre irrelevante, usada para reusarla.
   clear
   edit
   prompt 1 1 'Recepcion Dinero S/.'
   mask   6 3 'ZZZZZ9.99'.

SD NRO-TICKET-SD.
03 cNroDoc
   clear
   edit
   prompt  1, 'Ticket:      '
   mask   08, 'XXXXXXXX'.

SD RC-CODIGO-PRODUCTO-SD.
03 cCodPrd
   clear
   edit
   prompt  1, 'Cod.Prod.:     '
   mask   13, '999'.


SD MUESTRA-DESC-SD.
03 descri-mae-pro
   edit
   mask   17, 'XXXXXXXXXXXXXXXX'.

SD CANTIDAD-SD.
03 nCntPrd
   edit
   prompt 49, 'Cantid:        '
   mask   56, 'ZZZZZ9.99'.

SD MUESTRA-STOCK-SD.
03 nStock
   edit
   prompt 1, 3,  'Stock:          '
   mask     40,  'ZZZZZ9.99'.

SD DESC-DINERO-SD.
03 nCntDin
   edit
   clear
   prompt 17, 'Entrega Efectivo S/.:           '
   mask   38, 'ZZZ,ZZ9.99'.

SD GS-CONCEPTO-SD.
03 cConcepto
   auto-shift
   clear
   prompt  1, '    Concepto    '
   mask   1, 2 , 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

SD CS-PRODUCTO-SD.
03 descri-mae-pro
   clear
   mask 1, 'XXXXXXXXXXXXXXXX'.

03 vco-arc-inv
   edit
   prompt 17, 'Vts.Co:         '
   mask   24, 'ZZZZZ9.99'.

03 vcr-arc-inv
   edit
   prompt 33, 'Vts.Cr:         '
   mask   40, 'ZZZZZ9.99'.

03 nVentas
   edit
   prompt 49, 'Total :         '
   mask   56, 'ZZZZZ9.99'.


SD CS-PRESTAMOS-SD.
03 descri-mae-pro
   clear
  ;prompt 1, '   ENVASES:     '
   mask 1, 'XXXXXXXXXXXXXXXX'.

03 pen-arc-inv
   edit
   prompt 33, 'Presta:         '
   mask   40, 'ZZZZZ9.99'.

03 den-arc-inv
   edit
   prompt 49, 'Devolu:         '
   mask   56, 'ZZZZZ9.99'.


SD CS-CONSIGNAC-SD.
03 descri-mae-pro
   clear
  ;prompt 1, '   ENVASES:     '
   mask 1, 'XXXXXXXXXXXXXXXX'.

03 cso-arc-inv
   edit
   prompt 33, 'Cs.Oto:         '
   mask   40, 'ZZZZZ9.99'.

03 csr-arc-inv
   edit
   prompt 49, 'Cs.Rec:         '
   mask   56, 'ZZZZZ9.99'.


SD CS-STOCK.
03 descri-mae-pro
   clear
 ; prompt 1, '    STOCK:      '
   mask 1, 'XXXXXXXXXXXXXXXX'.
03 recarga-inv
   edit
   prompt 17, 'Recarg:         '
   mask   24, 'ZZZZZ9.99'.
03 ini-arc-inv
   edit
   prompt 33, 'StockI:         '
   mask   40, 'ZZZZZ9.99'.
03 canti-arc-inv
   edit
   prompt 49, 'Stock :         '
   mask   56, 'ZZZZZ9.99'.

SD DIA-1-SD.                   ;/Para dias de Visista de Cliente.
03 dia-1
   auto-shift
   auto-record
   edit
   mask 35, 'X'.

SD DIA-2-SD.
03 dia-2
   auto-shift
   auto-record
   edit
   mask 37, 'X'.

SD DIA-3-SD.
03 dia-3
   auto-shift
   auto-record
   edit
   mask 39, 'X'.

SD DIA-4-SD.
03 dia-4
   auto-shift
   auto-record
   edit
   mask 41, 'X'.

SD DIA-5-SD.
03 dia-5
   auto-shift
   auto-record
   edit
   mask 43, 'X'.

SD DIA-6-SD.
03 dia-6
   auto-shift
   auto-record
   edit
   mask 45, 'X'.

SD CS-DINERO-SD.
03 con-arc-car
   clear
   edit
   prompt  1, 'V.Cont:         '
   mask    8, 'ZZZZZ9.99'.

03 cre-arc-car
   edit
   prompt 17, 'V.Cred:         '
   mask   24, 'ZZZZZ9.99'.

03 ade-arc-car
   edit
   prompt 33, 'Adelan:         '
   mask   40, 'ZZZZZ9.99'.

03 cob-arc-car
   edit
   prompt 49, 'Cobran:         '
   mask   56, 'ZZZZZ9.99'.

SD CS-VISITAS-SD.
03 cli-can-car
   clear
   edit
   prompt 01, 'Visitados  :    '
   mask   14, 'ZZ9'.

03 cli-con-car
   edit
   prompt 17, 'Concretad  :    '
   mask   30, 'ZZ9'.

03 cli-ate-car
   edit
   prompt 33, 'Atendidos  :    '
   mask   46, 'ZZ9'.

03 cli-vis-car
   edit
   prompt 49, 'Nro.Visitas:    '
   mask   62, 'ZZ9'.

SD CS-TOTDIN-SD.
03 cini-arc-car
   edit
   prompt 33, 'CajIni:         '
   mask   40, 'ZZZZZ9.99'.
03 caja-arc-car
   edit
   prompt 49, 'TotDin:         '
   mask   56, 'ZZZZZ9.99'.

SD Dsp_Sld-SD.
03 nSldTot
   edit
   prompt 17, 'Credit:         '
   mask   24, 'ZZZZZ9.99'.
03 nSldCsg
   edit
   prompt 33, 'Consig:         '
   mask   40, 'ZZZZZ9.99'.
03 nSldEnv
   edit
   prompt 49, 'Envase:         '
   mask   56, 'ZZZZZ9.99'.




SD MENSAJE-CLI-SD.
03 mc-mensaje
   mask 17, 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

SD MEM-REM-S.
03 WS-MEM-AVAIL
   prompt 17, "           Bytes"
   mask   19, "ZZZZZ9"
   edit.

SD NUMERO-FAC-INI.
03 FACINI
   prompt 17, "Factura Inicial "
   mask   36, "99999999"
   edit.

SD NUMERO-BOL-INI.
03 BOLINI
   prompt 17, " Boleta Inicial "
   mask   36, "99999999"
   edit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                          CONFIGURACION IMPRESORA                           ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
COMMUNICATION section.

cd  PRINTER-COM-96                      ; Impresora IP-40.
    type is      BURST                  ; Star Micronics
    baud-rate is 9600
    data-bits    7
    parity       ODD
    mode is      ASCII
    block contains 40 bytes.

cd  PRINTER-COM-40                      ; Impresora MP-42.
    type is      BURST
    baud-rate is 9600
    data-bits    8
    parity       NONE
    mode is      ASCII
    block contains 32 bytes.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                             SECCION DE REPORTE                             ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
REPORT section.

rd  linea-print
    data linea1-ws
    data linea2-ws
    nlit X'0D0A'.

rd  CABECERA-01                                  ; Titulo 1
    lit ' '
    edit SIS-NOM-01 'XXXXXXXXXXXXXXXXXXXXXX'
;   edit SIS-NOM-02 'XX'
    lit ' Fecha : '
    edit nFecha '99/99/99'
    nlit X'1B0F'.

rd  CABECERA-02                                  ; Titulo 2
    lit ' Ruc.: '
    edit SIS-RUC-CIA '99999999'
    lit '      Hora  : '
    edit nHora '99:99:99'
    nlit X'0D0A'.

rd  CABECERA-RUC
    lit '             RUC: '
    edit SIS-RUC-CIA '99999999'
    nlit X'0D0A0D0A'.
;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                REPORTE DE INVENTARIO                                       ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
rd  CABECERA-03                                  ; Titulo 3
    lit '          INFORME DE INVENTARIO'
    nlit X'0D0A'
    lit '          ---------------------'
    nlit X'0D0A'.


rd  RECARGA-RD                                    ; Titulo 2
    lit 'Vendedor: '
    edit nCodVd        '99'
    lit ', '
    edit nom-vende-ws 'XXXXXXXXXXXXXXXX'
    nlit X'0D0A'
    lit ' '.


rd  CABECERA-04                                  ; Titulo 4
    lit '    Codigo   Descripcion     Cantidad'
    nlit X'0D0A'.



rd  DETALLE-01                                   ; Imprimo Detalle
    lit '     '
    edit CAMPO1 '999'                            ;editado del cขdigo
    lit '     '
    edit CAMPO2 'XXXXXXXXXXXXX'                  ;editado de la descripciขn
    lit '     '
    edit CAMPO3 'ZZZZ.Z9'                        ;editado de la cantidad
    nlit X'0D0A'.

rd  PIE-PAGINA-01
    lit '                              -------'
    nlit X'0D0A'
    lit '                   Totales:'.

rd  PIE-PAGINA-02
    lit ' '
    edit total-inve 'ZZZZ.Z9'
    nlit X'0D0A'
    nlit X'0D0A'
    nlit X'0D0A'
    lit '     --------------   --------------'
    nlit X'0D0A'
    lit '        Receptor        Deposito'.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                RECIBO DE DINERO                                            ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

rd  CAB-DINE-000
    nlit X'1B340E'
    lit '  RECIBO DE DINERO'
    nlit X'1B0F'
    nlit X'0D0A'.

rd  CAB-DINE-001
    lit ' Codigo Vendedor : '
    edit CAMPO50 '99'
    lit '   Nro.  : '
    edit CAMPO51 'ZZZZZZZ9'
    nlit X'0D0A'
    lit ' ---------------------------------------'
    nlit X'0D0A'
    lit '   Total Efectivo   S/. :  '
    edit CAMPO52 'ZZZ,ZZ9.99'
    nlit X'0D0A'
    nlit X'0D0A'.

rd  DET-DINE-001
    lit '               Cheques :'
    nlit X'0D0A'
    lit '   Serie     Codigo Bco.  Monto S/.'
    nlit X'0D0A'
    lit '  '
    edit CAMPO53 'XXXXXXXX'
    lit '       '
    edit CAMPO54 '99'
    lit '         '
    edit CAMPO55 'ZZZ,ZZ9.99'
    nlit X'0D0A'.

rd  DET-DINE-002
    lit '                        --------------'
    nlit X'0D0A'
    lit '                 Totales:  '
    edit CAMPO56 'ZZZ,ZZ9.99'
    nlit X'0D0A'
    nlit X'0D0A'
    lit '    --------------   --------------'
    nlit X'0D0A'
    lit '       Cajero(a)        Vendedor'
    nlit X'0D0A'.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                BOLETA Y FACTURA                                            ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

rd  CAB-FACT-000
    nlit X'1B341B0F'
    lit '   '
    edit SIS-NOM-01 'XXXXXXXXXXXXXXXXXXXXXX'
    edit SIS-NOM-02 'XXXXXXXXXXXXX'
    nlit X'0D0A'
    lit '   '
    edit SIS-DIR-01 'XXXXXXXXXXXXXXXXXXXX'
    edit SIS-DIR-02 'XXXXXXXXXXXXXXX'
    nlit X'0D0A'.

rd  CAB-FACT-001                                 ; Titulo 1
   ;nlit X'0D0A'
    lit '          '
    edit cComprobante  'XXXXXXXXXXXXXXXXXX'.

RD  CAB-FACT-002
    edit SIS-COD-02 '99'
    lit '0'
    edit w-01-fser '99'
    lit '-0'
    edit w-01-fcor '999999'
    nlit X'0D0A'
   ;lit '                    R.U.C.   : '
   ;edit SIS-RUC-CIA '99999999'
   ;nlit X'0D0A'
    lit '                         FECHA : '
    edit p-01-date '99/99/99'
    nlit X'0D0A'.

rd  CAB-FACT-003                                 ; Titulo 2
    lit 'Codigo : '
    edit p-01-codigo    '999999'
    lit '          HORA  : '
    edit p-01-time  '99:99:99'
    nlit X'0D0A'
    lit 'Nombre : '
    edit p-01-nomb-esta 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
    nlit X'0D0A'
    lit 'Direcci: '
    edit p-01-direccion 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
    nlit X'0D0A'.

rd  CAB-FACT-006                                 ; Titulo 2
    lit 'R.U.C. : '
    edit p-01-ruc   '99999999'
    nlit X'0D0A'.

rd  CAB-FACT-007                                 ; Titulo 2
    lit 'Maquina: '
    edit p-01-vendedor '99'
    lit ', '
    edit nom-vende-ws 'XXXXXXXXXXXXXXXX'
    nlit X'0D0A'.

rd  CAB-FACT-008                                 ; Titulo 3
    lit 'Doc.Ref.Nro.: '
    edit cNroDoc  '99999999'
    nlit X'0D0A'.

rd  RAYA
    lit '  ---------------------------------------'
    nlit X'0D0A'.

rd  CAB-FACT-005
    lit 'Descripcion    Cantidad P.Unit.     Total'
    nlit X'0D0A'.

rd  DET-FACT-001
 ;  lit ' '
    edit p-02-des-prod 'XXXXXXXXXXXXXXX'         ; Descripciขn del Producto
    lit ' '
    edit p-02-can-prod 'ZZZ9.99'                 ; Cantidad del Producto
    lit ' '
    edit p-02-pre-prod 'Z9.99999'                ; Precio del Producto
    lit ' '
    edit p-02-par-prod 'ZZZZ9.99'                ; Cantidad Parcial.
    nlit X'0D0A'.

rd  DET-FACT-001-1
    edit p-02-des-prod 'XXXXXXXXXXXXXXX'         ; Descripciขn del Producto
    lit ' '
    edit p-02-can-prod 'ZZZZZ9.99'                 ; Cantidad del Producto
    nlit X'0D0A'.

rd  DET-FACT-002
    lit '               Valor de Venta : '
    edit CAMPO14 'ZZZZZ9.99'                     ; Total Neto de la Venta
    nlit X'0D0A'
    lit '               I.G.V.         : '
    edit CAMPO15 'ZZZZZ9.99'                     ; I.G.V.
    nlit X'0D0A'.

rd  DET-FACT-002-1
    lit '               Sub-Total      : '
    edit CAMPO17 'ZZZZZ9.99'                     ; Total Neto de la Venta
    nlit X'0D0A'
    lit '               Descuento      : '
    edit CAMPO18 'ZZZZZ9.99'                     ; Total Neto de la Venta
    nlit X'0D0A'
    lit '               Valor de Venta : '
    edit CAMPO14 'ZZZZZ9.99'                     ; Total Neto de la Venta
    nlit X'0D0A'
    lit '               I.G.V.         : '
    edit CAMPO15 'ZZZZZ9.99'                     ; I.G.V.
    nlit X'0D0A'.

rd  Cr_Lf
    nlit X'0D0A'.

rd  DET-FACT-005
    lit '               Precio de Venta: '
    edit CAMPO16 'ZZZZZ9.99'                     ; Total Venta
    nlit X'0D0A'.

rd  DET-FACT-006
    lit '               Deecuento      : '
    edit p-01-monto-dsc  'ZZZZZ9.99'                     ; Total Venta
    nlit X'0D0A'.

rd  DET-FACT-003
    nlit X'0D0A'
    lit 'Condicion :'
    edit var6-ws 'XXXXXXX'
    nlit X'0D0A'.

rd  DET-FACT-003-1
    lit 'Pago adelantado:'
    edit p-01-monto-adelan 'ZZZZZ9.99'
    lit ' Saldo:'
    edit nSaldo        'ZZZZZ9.99'
    nlit X'0D0A'.


  ;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
  ;ณ   Emisiขn de Recibo de Cobranza.                         ณ
  ;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

rd  CAB-COBR-001
    lit '            RECIBO DE COBRANZA '
    nlit X'0D0A'
    lit '            -------------------'
    nlit X'0D0A'.

rd  CAB-COBR-002
    lit ' Numero: '
    edit p-12-documento '99999999'
    nlit X'0D0A'
    lit ' Referencia Nro.Doc.: '
    edit p-12-factura '99999999'
    nlit X'0D0A'
    lit ' Codigo Cliente : '
    edit p-12-cod-cli '999999'
    nlit X'0D0A'
    lit ' Nombre : '
    edit p-12-nom-cli 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
    nlit X'0D0A'
    lit ' Direcc.: '
    edit p-12-dir-cli 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
    nlit X'0D0A'
    lit ' Fecha  : '
    edit p-12-date '99/99/99'
;   nlit X'0D0A'
    lit ' Hora   : '
    edit p-12-time '99:99:99'
    nlit X'0D0A'
    lit ' Importe: '
    edit p-12-monto  'ZZZZZ9.99'
    nlit X'0D0A'
    lit ' Saldo  : '
    edit nSaldo  'ZZZZZ9.99'
    nlit X'0D0A'.

rd  CAB-COBR-003
    lit ' Cheque : '
    edit cSerieBanco  'XXXXXXXX'
    nlit X'0D0A'.

 ;------------------- Fin Emisi•n de Recibo de Cobranza ----------------------

rd  CAB-MUES-001
    nlit X'1B340E'
    lit '      MUESTREO'
    nlit X'0D0A'
    nlit X'1B0F'
    lit 'Codigo  Nombre           Estado'
    nlit X'0D0A'
    lit 'Factura     Hora   Direccion'
    nlit X'0D0A'
    nlit X'0D0A'.

rd  DET-MUES-001
    edit p-06-codigo '999999'
    lit '  '
    edit p-06-nomb-esta 'XXXXXXXXXXXXXXX'
    lit '  '
    edit p-06-estado 'XXXXXXXXXXXXXXX'
    nlit X'0D0A'
    edit p-06-factura 'XXXXXXXX'
    lit ' '
    edit p-06-time '99:99:99'
    lit '  '
    edit p-06-direccion 'XXXXXXXXXXXXXXXXXXXX'
    nlit X'0D0A'
    nlit X'0D0A'.

rd  CAB-INFO-001
    nlit X'1B340E'
    lit ' INFORME SUPERVISOR'
    nlit X'0D0A'
    nlit X'1B0F'
    lit 'Visitas a Realizar   :  '
    edit tot1-ws 'ZZ9'
    nlit X'0D0A'
    lit 'Numero de Visitas    :  '
    edit tot2-ws 'ZZ9'
    nlit X'0D0A'
    lit 'Ventas  Realizadas   :  '
    edit tot3-ws 'ZZ9'
    nlit X'0D0A'
    lit 'Cobranzas Realizadas :  '
    edit tot4-ws 'ZZ9'
    nlit X'0D0A'
    lit '               Inventario'
    nlit X'0D0A'
    lit 'Codigo Descripcion    Stock Inicial'
    nlit X'0D0A'
    lit 'Ventas   Stock Final'
    nlit X'0D0A'
    nlit X'0D0A'.

rd  DET-INFO-001
    edit p-07-codigo '999'
    lit '  '
    edit p-07-descri 'XXXXXXXXXXXXXXXX'
    lit '  '
    edit p-07-inicial 'ZZZZZ9'
    nlit X'0D0A'
    edit p-07-ventas 'ZZZZZ9'
    lit ' '
    edit p-07-final 'ZZZZZ9'
    nlit X'0D0A'.

rd  DET-INFO-002
    lit 'V. B. Supervisor : _____________'
    nlit X'0D0A'
    nlit X'0D0A'.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                REPORTE DE LIQUIDACION                                      ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

rd  CAB-FIN-DIA-001
    ;nlit X'1B340E'
    lit 'INFORME FIN DE DIA'
    nlit X'0D0A'
    lit 'Vendedor :  '
    edit nom-vende-ws 'XXXXXXXXXXXXXXXX'
    nlit X'0D0A'
    nlit X'0D0A'
    lit '              Visitas      '
    nlit X'0D0A'
    lit 'Clientes Visitados   :  '
    edit cli-can-car 'ZZ9'
    nlit X'0D0A'
    lit 'Clientes Atendidos   :  '
    edit cli-ate-car 'ZZ9'
    nlit X'0D0A'
    lit 'Numero de Visitas    :  '
    edit cli-vis-car 'ZZ9'                ;Clientes Concretados.cli-con-car
    nlit X'0D0A'                          ;Nro.Visitas.         cli-vis-car
    lit 'Ventas  Concretadas  :  '        ;Clientes Atendidos.  cli-ate-car
    edit cli-con-car  'ZZ9'               ;Clientes Visitados.  cli-can-car
    nlit X'0D0A'.

   ;nlit X'1B0F'
   ;lit 'Visitas a Realizar   :  '
   ;edit tot1-ws 'ZZ9'
   ;nlit X'0D0A'                                   ;Caja Inicial

rd  DET-FIN-DIA-002
    lit 'Caja Inicial   S/.: '
    edit cini-arc-car 'ZZZ,ZZ9.99'
    nlit X'0D0A'
    lit 'Monto Cobranza S/.: '             ;Caja Final           caja-arc-car
    edit cob-arc-car 'ZZZ,ZZ9.99'          ;Cobranzas            cob-arc-car
    nlit X'0D0A'                           ;Crditos             cre-arc-car
    lit 'Venta Contado  S/.: '             ;Contados             con-arc-car
    edit con-arc-car 'ZZZ,ZZ9.99'
    nlit X'0D0A'
    lit 'Monto Adelanto S/.: '
    edit ade-arc-car 'ZZZ,ZZ9.99'
    nlit X'0D0A'
    lit 'Caja Final     S/.: '
    edit caja-arc-car 'ZZZ,ZZ9.99'
    nlit X'0D0A'
    nlit X'0D0A'
    lit 'Venta Credito  S/.: '
    edit cre-arc-car 'ZZZ,ZZ9.99'
    nlit X'0D0A'
    nlit X'0D0A'.

rd  CAB-FIN-DIA-FOL
    lit '   Series de Documentos    '
    nlit X'0D0A'
    lit ' Modulo       Doc.Inicial  Doc.Final  Dif.'
    nlit X'0D0A'.


rd  DET-FIN-DIA-FOL
    edit folio-tab-dsc 'XXXXXXXXXXXXXXXX'
    lit ' :'
    edit folio-tab-ini '99999999'
    lit ' '
    edit nNroDoc       '99999999'
    lit ' '
    edit nEntero      '9999'
    nlit X'0D0A'.


rd INVENTARIO
    lit '               INVENTARIO'
    nlit X'0D0A'
    lit 'Codigo Descripcion    '
    nlit X'0D0A'.

rd  Inv_Fin_Dia
    edit codigo-arc-inv '999'
    lit '  '
    edit descri-mae-pro 'XXXXXXXXXXXXXXXX'
    nlit X'0D0A'

    lit '     Stock Inicial   :'
    edit ini-arc-inv 'ZZZZ.Z9'
    nlit X'0D0A'.

rd  Inv_Fin_Dia-1
    lit '     Ventas Contado  :'
    edit vco-arc-inv 'ZZZZ.Z9'
    nlit X'0D0A'.


rd  Inv_Fin_Dia-2
    lit '     Ventas Credito  :'
    edit vcr-arc-inv 'ZZZZ.Z9'
    nlit X'0D0A'.

rd  Inv_Fin_Dia-3
    lit '     Prestamo Envases:'
    edit pen-arc-inv 'ZZZZ.Z9'
    nlit X'0D0A'.

rd  Inv_Fin_Dia-4
    lit '     Devolucion Envas:'
    edit den-arc-inv 'ZZZZ.Z9'
    nlit X'0D0A'.

rd  Inv_Fin_Dia-5
    lit '     Consig. Otorgada:'
    edit cso-arc-inv 'ZZZZ.Z9'
    nlit X'0D0A'.

rd  Inv_Fin_Dia-6
    lit '     Consig. Recogida:'
    edit csr-arc-inv 'ZZZZ.Z9'
    nlit X'0D0A'.

rd  Inv_Fin_Dia-7
    lit '     Recargas        :'
    edit recarga-inv 'ZZZZ.Z9'
    nlit X'0D0A'.

rd  Inv_Fin_Dia-8
    lit '     Obsequios       :'
    edit obs-arc-inv 'ZZZZ.Z9'
    nlit X'0D0A'.

rd  Inv_Fin_Dia-9
    lit '     Descargas       :'
    edit dsc-arc-inv 'ZZZZ.Z9'
    nlit X'0D0A'.

rd  Inv_Fin_Dia-10
    lit '     Canjes          :'
    edit cjs-arc-inv 'ZZZZ.Z9'
    nlit X'0D0A'.

rd  Inv_Fin_Dia-11
    lit '     Roturas         :'
    edit rts-arc-inv 'ZZZZ.Z9'
    nlit X'0D0A'.

rd  Inv_Fin_Dia-12
    lit '     Stock Final     :'
    edit canti-arc-inv 'ZZZZ.Z9'
    nlit X'0D0A'.


rd  DET-FIN-DIA-003
    lit 'Observacion : _____________________'
    nlit X'0D0A'
    nlit X'0D0A'
    lit '___________________________________'
    nlit X'0D0A'
    nlit X'0D0A'
    lit 'Vendedor  : _____________'
    nlit X'0D0A'
    nlit X'0D0A'
    lit 'Recepcion : _____________'
    nlit X'0D0A'
    nlit X'0D0A'.

rd  FIN-LISTADO                                  ; Indicador de fin de listado
    nlit X'0D0A'
    lit '               --- O ---'
    nlit X'0D0A'
    nlit X'0D0A'
    nlit X'0D0A'.

rd  COMPR-VARIOS-00
    lit '          COMPROBANTE '
    lit 'DE ENTREGA'
    nlit X'0D0A'.

rd  COMPR-VARIOS-01
    lit '            '
    lit 'Documento: '
    edit nro-doc-cab-var '99999999'
    nlit X'0D0A'.

rd  COMPR-VARIOS-02
    nlit X'0D0A'
    lit 'Cliente: '
    data nom-cab-var-01
    edit nom-cab-var-02 'XXXXXXXXXXXX'
    nlit X'0D0A'.

rd  COMPR-VARIOS-03
    lit 'Motivo : '
    data motivo-cab-var
    nlit X'0D0A'.

rd  COMPR-VARIOS-04
    lit 'Dinero entregado por Cliente:'
    nlit X'0D0A'
    lit '          S/. '
    edit dinero-cab-var 'ZZZ,ZZ9.99'
    nlit X'0D0A'.

rd  COMPR-VARIOS-05
    lit '       '
    data descri-mae-pro
    lit '     '
    edit canti-det-var 'ZZZ9.99'
    nlit X'0D0A'.

  ;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
  ;ณ   Emisiขn de Liquidaciขn Diaria.                         ณ
  ;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

rd  CAB-DV-001
    lit '            RECIBO DE DEVOLUCION '
    nlit X'0D0A'
    lit '                DE ENVASES       '
    nlit X'0D0A'
    lit '            --------------------'
    nlit X'0D0A'.

rd  CAB-DV-002
    lit ' Numero : '
    edit p-09-documento '99999999'
    nlit X'0D0A'
    lit ' Referencia Nro.Doc.: '
    edit p-09-doc-pre   '99999999'
    nlit X'0D0A'
    lit ' Codigo Cliente : '
    edit p-09-cod-cli 'ZZZZZ9'
    nlit X'0D0A'
    lit ' Nombre : '
    edit p-09-nom-cli 'XXXXXXXXXXXXXXXXXXXXXXXXX'
    nlit X'0D0A'
    lit ' Direcc.: '
    edit p-09-dir-cli 'XXXXXXXXXXXXXXXXXXXXXXXXX'
    nlit X'0D0A'
    lit ' Fecha  : '
    edit p-09-date '99/99/99'
    nlit X'0D0A'
    lit ' Hora   : '
    edit p-09-time '99:99:99'
    nlit X'0D0A'
   ;lit ' Codigo     Descripcion     Cantidad '
    lit 'Codigo   Descripcion   Cantidad  Saldo  '
    nlit X'0D0A'.
   ;lit ' ---------------------------------------'
   ;nlit X'0D0A'.

rd  CAB-DV-003
    lit ' '
    edit p-09-cod-prod '999'
    lit ' '
    edit p-09-dsc-prod 'XXXXXXXXXXXXXXXX'
    lit ' '
    edit p-09-cnt-prod 'ZZZ9.99'
    lit ' '
    edit p-09-cnt-sld  'ZZZ9.99'
    nlit X'0D0A'.

  ;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
  ;ณ   Emisiขn de Devolucion de Consignacion                  ณ
  ;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

rd  CAB-CSDV-001
    lit '    RECIBO DE DEVOLUCION DE CONSIGNACION'
    nlit X'0D0A'
    lit '              SIN ENVASES            '
    lit '     --------------------------------'
    nlit X'0D0A'.

rd  CAB-CSDV-001-1
    lit '    RECIBO DE DEVOLUCION DE CONSIGNACION'
    nlit X'0D0A'
    lit '              CON ENVASES            '
    lit '     --------------------------------'
    nlit X'0D0A'.

rd  CAB-CSDV-002
    lit ' Numero: '
    edit p-15-documento  '99999999'
    nlit X'0D0A'
    lit ' Referencia Nro.Doc.: '
    edit p-15-doc-pre   '99999999'
    nlit X'0D0A'
    lit ' Codigo Cliente : '
    edit p-15-cod-cli 'ZZZZZ9'
    nlit X'0D0A'
    lit ' Nombre : '
    edit p-15-nom-cli 'XXXXXXXXXXXXXXXXXXXXXXXXX'
    nlit X'0D0A'
    lit ' Direcc.: '
    edit p-15-dir-cli 'XXXXXXXXXXXXXXXXXXXXXXXXX'
    nlit X'0D0A'
    lit ' Fecha  : '
    edit p-15-date '99/99/99'
    nlit X'0D0A'
    lit ' Hora   : '
    edit p-15-time '99:99:99'
    nlit X'0D0A'
    lit ' Codigo     Descripcion     Cantidad '
    nlit X'0D0A'.
   ;lit ' ---------------------------------------'
   ;nlit X'0D0A'.

rd  CAB-CSDV-003
    lit '   '
    edit p-15-cod-prod '999'
    lit '  '
    edit p-15-dsc-prod 'XXXXXXXXXXXXXXXX'
    lit '  '
    edit p-15-cnt-prod 'ZZZZ.Z9'
    nlit X'0D0A'.


  ;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
  ;ณ   Emisiขn de Recibo de Prstamo.                         ณ
  ;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

rd  CAB-PR-001
    lit '            RECIBO DE PRESTAMO '
    nlit X'0D0A'
    lit '            -------------------'
    nlit X'0D0A'.

rd  CAB-PR-002
    lit ' Numero : '
    edit p-08-documento '99999999'
    nlit X'0D0A'
    lit ' Codigo Cliente : '
    edit p-08-cod-cli 'ZZZZZ9'
    nlit X'0D0A'
    lit ' Nombre : '
    edit p-08-nom-cli 'XXXXXXXXXXXXXXXXXXXXXXXXX'
    nlit X'0D0A'
    lit ' Direcc.: '
    edit p-08-dir-cli 'XXXXXXXXXXXXXXXXXXXXXXXXX'
    nlit X'0D0A'
    lit ' Fecha  : '
    edit p-08-date '99/99/99'
    nlit X'0D0A'
    lit ' Hora   : '
    edit p-08-time '99:99:99'
    nlit X'0D0A'
    lit ' Codigo     Descripcion     Cantidad '
    nlit X'0D0A'.
   ;lit ' ---------------------------------------'
   ;nlit X'0D0A'.

rd  CAB-PR-003
    lit '   '
    edit p-08-cod-prod '999'
    lit '  '
    edit p-08-dsc-prod 'XXXXXXXXXXXXXXXX'
    lit '  '
    edit p-08-cnt-prod 'ZZZZ.Z9'
    nlit X'0D0A'.

rd  FIRMA-VENDEDOR
    nlit X'0D0A0D0A0D0A'
    lit '              --------------'
    nlit X'0D0A'
    lit '                 Vendedor'
    nlit X'0D0A'.

rd  FIRMA-VEND-CLIENTE
    nlit X'0D0A0D0A0D0A'
    lit '  --------------'
    lit '         --------------'
    nlit X'0D0A'
    lit '     Cliente'
    lit '                Vendedor'
    nlit X'0D0A'.

  ;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
  ;ณ   Emisiขn de Recibo de Consigancion Otorgada             ณ
  ;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

rd  CAB-CS-001
    lit '     RECIBO DE CONSIGNACION  OTORGADA'
    nlit X'0D0A'
    lit '              SIN ENVASES            '
    lit '     --------------------------------'
    nlit X'0D0A'.

rd  CAB-CS-001-1
    lit '     RECIBO DE CONSIGNACION  OTORGADA'
    nlit X'0D0A'
    lit '              CON ENVASES            '
    lit '     --------------------------------'
    nlit X'0D0A'.

rd  CAB-CS-002
    lit ' Numero : '
    edit p-13-documento '99999999'
    nlit X'0D0A'
    lit ' Codigo Cliente : '
    edit p-13-cod-cli 'ZZZZZ9'
    nlit X'0D0A'
    lit ' Nombre : '
    edit p-13-nom-cli 'XXXXXXXXXXXXXXXXXXXXXXXXX'
    nlit X'0D0A'
    lit ' Direcc.: '
    edit p-13-dir-cli 'XXXXXXXXXXXXXXXXXXXXXXXXX'
    nlit X'0D0A'
    lit ' Fecha  : '
    edit p-13-date '99/99/99'
    nlit X'0D0A'
    lit ' Hora   : '
    edit p-13-time '99:99:99'
    nlit X'0D0A'
    lit ' Codigo     Descripcion     Cantidad '
    nlit X'0D0A'.
   ;lit ' ---------------------------------------'
   ;nlit X'0D0A'.

rd  CAB-CS-003
    lit '   '
    edit p-13-cod-prod '999'
    lit '  '
    edit p-13-dsc-prod 'XXXXXXXXXXXXXXXX'
    lit '  '
    edit p-13-cnt-prod 'ZZZZ.Z9'
    nlit X'0D0A'.

rd  CAB-CS-004
    lit 'Condicion:'
    edit cComprobante 'XXXXXXXXXXXXXXXXX'
    nlit X'0D0A'.



  ;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
  ;ณ   Emisiขn de Recibo de Obsequio.                         ณ
  ;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

rd  CAB-OB-001
    lit '            RECIBO DE OBSEQUIO '
    nlit X'0D0A'
    lit '            -------------------'
    nlit X'0D0A'.

rd  CAB-OB-002
    lit ' Numero : '
    edit Ob-NroDoc '99999999'
    nlit X'0D0A'
    lit ' Fecha  : '
    edit nFecha '99/99/99'
    nlit X'0D0A'
    lit ' Hora   : '
    edit nHora   '99:99:99'
    nlit X'0D0A'
    lit ' Codigo     Descripcion     Cantidad '
    nlit X'0D0A'.
   ;lit ' ---------------------------------------'
   ;nlit X'0D0A'.

rd  CAB-OB-003
    lit '   '
    edit Ob-CodPrd     '999'
    lit '  '
    edit descri-mae-pro 'XXXXXXXXXXXXXXXX'
    lit '  '
    edit Ob-CntPrd     'ZZZ9.99'
    nlit X'0D0A'.



rd  CAB-DS-001
    lit '            RECIBO DE DESCARGAS  '
    nlit X'0D0A'
    lit '            -------------------'
    nlit X'0D0A'
    lit ' '
    nlit X'0D0A'.


rd  CAB-DS-002
    lit ' Vendedor :'
    edit nom-vende-ws 'XXXXXXXXXXXXXXXX'
    nlit X'0D0A'
    lit ' Documento:'
    edit nNroDoc '99999999'
    nlit X'0D0A'
    lit ' -------------------------------'
    nlit X'0D0A'.

rd  CAB-DS-003
    lit '  '
    edit p-11-cod-prod '999'
    lit '  '
    edit p-11-dsc-prod 'XXXXXXXXXXXXXXXX'
    lit '  '
    edit p-11-cnt-prod 'ZZZ9.99'
    nlit X'0D0A'.

rd  CAB-DS-004
    nlit X'0D0A'
    nlit X'0D0A'
    lit '    --------------   --------------'
    nlit X'0D0A'
    lit '       Almacenero       Vendedor'
    nlit X'0D0A'.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                              PROCEDURE DIVISION                            ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
PROCEDURE DIVISION.
DECLARATIVES.
DEC-1.
    when OFF   POWER-OFF
               move 0 to flag-hora
               backlight-off
               if prende-apaga = 0 then
                  perform POWER-MESSAGE.
         end-when
    when TMOUT alarm
               move 0 to flag-hora
               backlight-off
               power-off
               if prende-apaga = 0 then
                  perform POWER-MESSAGE.
         end-when
    when PROC  move DISPLAY-BUFFER to resp-pantalla
               show PROC-SD
               message 1,  '****************'
               message 49, '****************'
               wait 1 second
               move resp-pantalla to DISPLAY-BUFFER.
    when ADJ   adjust-lcd UP.
    when LIGHT perform TURN-LIGHT-ON-OFF.
    when KBEEP perform AJUSTA-BEEP.
    when KMEM  perform CHECK-MEMORY.
    when YES   CLEAR RETURN EXIT.
    when KNOTA perform AJUSTA-TONO.
    when KVER  perform SaveScreen
               perform Version
               perform RestScreen.
    when KVEND perform SaveScreen
               perform Muestra_Vendedor
               perform RestScreen.
    when KMENU if nProcAct = 0 and prende-ws-1 = 0
                  move DISPLAY-BUFFER to resp-pantalla
                  move 1 to nProcAct
                  perform CONSULTAS
                  move 0 to nProcAct
                  move resp-pantalla to DISPLAY-BUFFER.
    end-when

end declaratives.

INITIAL SECTION.
INIT-SYSTEM.
;-----------
    set-keyboard STD-710
    SET-DECLARATIVES DEC-1
   ;MOVE X'33CC009C636363' TO WS-CHAR-DEF   ;ค
   ;SPECIAL-CHAR IS 164 USING WS-CHAR-DEF


    move 700                to user-tmout.
    move ' Inicializacion ' to proc-ws.
    move 5                  to fast-speed.
    move 15                 to scroll-speed.
    move '@'                to shift-char.
    move 0                  to flag-hora.
    move 1                  to tecla-beep.
    move 25                 to tecla-tono.
    move 0                  to nProcAct
    move 0                  to nPanCons
    move 1                  to punt-consulta
    move 0                  to nPanCli
    perform ini-ram-ptc

    read-clock.                                  ; inicio hora en 24 hrs
    move SY-DATE-TIME to fecha-ws.
    move hrs-ws       to hrs.
    move xxxxx-ws     to rr.
    if rr = 1 and hrs < 12
       add 12         to hrs.
    move hrs          to hrs-ws.
    move 1            to yyyyy-ws.
    move fecha-ws     to SY-DATE-TIME.
    set-clock.

    perform POWER-MESSAGE.

INIT-010.
;--------
    perform MENU-PRINCIPAL
    loop.

POWER-MESSAGE.
;-------------
    move 700                to user-tmout
    move DISPLAY-BUFFER to resp-pantalla
    move 1 to prende-apaga
    perform OPENREF-IDENTIF-F
         when ER  perform CARGA-DATOS-ALL
                  loop.
    seek FIRST in IDENTIFICA-F
         when EOF message  1, '****************'
                  message 17, 'Archivo Identif.'
                  message 33, '     Vacio      '
                  message 49, '****************'
                  perform lee-tecla-action
                  perform CARGA-DATOS-ALL
                  loop.
    read IDENTIFICA-F
    move cod-arc-ide to nCodVd
    move nom-arc-ide to nom-vende-ws

POWER-MESSAGE-001.
;-----------------
    move 5         to ws-prende-ptc
    move ' '       to shift-char
    message 01  '*  EXCEL S.A   *'.
    message 17, '****************'.
    message 33, ' Ingrese  Clave '.
    message 49  '                '.
    accept PRENDE-PTC-SD
       when OK perform LeeHoraFecha
               seek FIRST in IDENTIFICA-F
               search FIRST for ws-prende-ptc in IDENTIFICA-F offset is password-ide
                   when ER beep
                           message 01, '****************'
                           message 17, '**   Acceso   **'
                           message 33, '**  Denegado  **'
                           message 49, '****************'
                           wait 1 second
                           loop.
                   when OK read IDENTIFICA-F
                           if prende-ws-1 = 0
                              move cod-arc-ide to nCodVd
                              move nom-arc-ide to nom-vende-ws
                           end-if
                           move resp-pantalla to DISPLAY-BUFFER
                           next.
            end-when
    loop.

POWER-MESS-002.
;--------------
    move '@' to shift-char.
    move 0   to prende-apaga.
;   clear.
    exit.

Carga_Vendedor.
;--------------
if ws-prende-ptc > 0
   exit.
search FIRST for ws-prende-ptc in IDENTIFICA-F offset is menu-arc-ide
   when Ok READ IDENTIFICA-F.


Muestra_Vendedor.
;-----------------
 perform Lee-Nom-Vendedor
 perform LeeHoraFecha
 message 01  '                '
 show COD-VENDE-SD
 message 17, '.              .'
 show NOM-VENDEDOR-SD
 message 33, '.              .'
 show FECHA-HOY-SD
 message 49  '                '
 show HORA-HOY-SD
 WAIT 1 SECOND
exit.

Version.
;------
 message 01  '................'
 message 17, '.    Ventas    .'
 message 33, '.    V02.001   .'
 message 49  '................'
 WAIT 1 SECOND
exit.


ini-ram-ptc.
;----------
    perform detecta-vendedor
   ;perform lee-nom-vendedor
    perform ini-cab-documentos
    move nCodVd       to carga-codigo
exit.

lee-nom-vendedor.
;----------------
perform OPENREF-IDENTIF-F
move 0 to prende-ws-1
search FIRST for prende-ws-1 in IDENTIFICA-F offset is password-ide
       when OK READ IDENTIFICA-F
               move nom-arc-ide to nom-vende-ws.
exit.



detecta-vendedor.
;----------------
    move 0 to nCodVd      .
    find first busca-i
    when er exit.
    move file-name to ws-file-name
    if nombre-fn = 'IDENTI'
       move codigo-fn to nCodVd
       exit.
    perform detecta-vend-loop
loop.

detecta-vend-loop.
;-----------------
    find next busca-i
    when er exit.
    move file-name to ws-file-name
    if nombre-fn = 'IDENTI'
       move codigo-fn to nCodVd
       exit.
loop.

ini-cab-documentos.
;-----------------
   fill REG-SISTEMA with ' '
   perform openref-sistema-f
        when er exit.
   seek first in sistema-f
        when eof exit.
   read sistema-f
exit.

; ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
; ณ                               MENU  DE CONSULTAS                         ณ
; ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
CONSULTAS.
;---------

clear
message 01, "  CONSULTAS    "
message 18, "PRODUCTOS      "
message 34, "DINERO         "
message 50, "VISITAS        "
next.


CONSULTAS01.
;-----------
    examine punt-consulta
            when  0 move 3 to punt-consulta  loop.
            when  1 message 17, ">"
                    message 33, " "
                    message 49, " "
                    fast-action.
            when  2 message 17, " "
                    message 33, ">"
                    message 49, " "
                    fast-action.
            when  3 message 17, " "
                    message 33, " "
                    message 49, ">"
                    fast-action.

            when  4 move 1 to punt-consulta  loop.
    no-op.
            when KNEXT  subtract 1 from punt-consulta.
            when FAR    add 1 to punt-consulta.
            when BAR    subtract 1 from punt-consulta.
            when KEND   exit.
            when ENTER  if punt-consulta = 1
                           perform CS-PRODUCTOS
                        else if punt-consulta = 2
                               move DISPLAY-BUFFER to resp-pantalla1
                               perform CS-DINERO
                               move resp-pantalla1 to DISPLAY-BUFFER
                             else
                                move DISPLAY-BUFFER to resp-pantalla1
                                perform CS-VISITAS
                                move resp-pantalla1 to DISPLAY-BUFFER
                             end-if
                        end-if
            end-when

    loop.

CS-DINERO.
;---------
perform Caj_Ini

CS-DINERO01.
;-----------
show CS-DINERO-SD
action
   when KEND  exit.
   when KNEXT SHOW CS-TOTDIN-SD
              WAIT 2 SECOND.
loop.

CS-VISITAS.
;---------
perform Caj_Ini

CS-VISITAS01.
;-----------
show CS-VISITAS-SD
action
   when KEND  exit.
loop.


CS-PRODUCTOS.
;------------
perform OPENREF-MAE-PRO-F
      when ER exit.
SEEK FIRST in MAE-PRO-F
      when EOF exit.
READ MAE-PRO-F

move DISPLAY-BUFFER to resp-pantalla1
perform CS-RECORRE-PRODUCTOS
move resp-pantalla1 to DISPLAY-BUFFER.
exit.


CS-RECORRE-PRODUCTOS.
;--------------------
 move codigo-mae-pro   to  cCodPrd         ; Para la pantalla

 perform  Cs_Busca_Producto
          when ER exit.

 examine  nPanCons
          when 0 show CS-STOCK.
          when 1 show CS-PRODUCTO-SD.
          when 2 show CS-PRESTAMOS-SD.
          when 3 show CS-CONSIGNAC-SD.
          when 4 move 0 to nPanCons loop.
 action
 when KFIND perform ACEPTA-PRODUCTO.
 when KNEXT add 1 to nPanCons.
 when DAR   perform AVANZA-PRODUCTO.
 when UAR   perform RETROC-PRODUCTO.
 when KEND  exit.
 loop.

Cs_Busca_Producto.
;-----------------
perform OPENREF-ARC-INV
     when ER exit.
seek FIRST in ARC-INV-F
     when EOF exit.
search FIRST for cCodPrd in ARC-INV-F
    when ER move 0 to vco-arc-inv
            move 0 to vcr-arc-inv
            move 0 to pen-arc-inv
            move 0 to csr-arc-inv
            move 0 to cso-arc-inv
            move 0 to den-arc-inv
            move 0 to obs-arc-inv
            move 0 to rts-arc-inv
            move 0 to cjs-arc-inv
            move 0 to dsc-arc-inv
            move 0 to Ini-arc-inv
            move 0 to recarga-inv
            move 0 to canti-arc-inv
            move 0 to nVentas
            move OK to last-key.

    when OK read ARC-INV-F
            move vco-arc-inv   to     nNumero1       ; Suma
            move vcr-arc-inv   to     nNumero2
            perform Iv_Suma_Enteros
            move nNumero1      to     nVentas
    end-when
exit.



;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                                MENU PRINCIPAL                              ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
MENU-PRINCIPAL SECTION.
;----------------------
    clear
    move 1 to punt-mp
    if prende-ws-1 = 0
       perform HAY-IMPRESORA
       ;if punt-impresora = 2
       ;   perform PIDE-FACBOL-INIC
       ;end-if
       perform MENU-VENDEDOR
    end-if
    if prende-ws-1 = 1
      ;perform MENU-SUPERVISOR
       perform SELECCIONA-MENU
    end-if
    if prende-ws-1 = 2
       move 1 to punt-mp
       perform MENU-OPERADOR
    end-if
    if prende-ws-1 = 3
       perform MENU-ALMACENERO
    end-if
    exit.


;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                 RUTINA PARA ELEGIR SI HAY IMPRESORA O NO                   ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
HAY-IMPRESORA.
;-------------
    move 'CSIMPRE' to file-name
    open REF CTR-IMP-F
         when ER create CTR-IMP-F
             open UPDATE CTR-IMP-F
             move 'N' to flag-impresora
             write REG-CTR-IMP
             open REF CTR-IMP-F
         end-when
    seek first in CTR-IMP-F
    read CTR-IMP-F
    exit.





;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ             RUTINA PARA INGRESAR LA PRIMERA BOLETA Y FACTURA               ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
PIDE-FACBOL-INIC.
;----------------
;   message 1,  '****************'.
;   message 33, '                '.
;   message 49, '****************'.
;   accept NUMERO-FAC-INI.
;   accept NUMERO-BOL-INI.
;
;   OPENREF-TAB-FOL-F.
;
;   ; actualizamos en el archivo de folios la primera factura del dกa
;   move 1 to codigo-tab-fol
;   search FIRST for codigo-tab-fol in TAB-FOL-F
;   read TAB-FOL-F
;   move facini to folio-tab-fol
;   rewrite REG-TAB-FOL
;
;   ; actualizamos en el archivo de folios la primera factura del dกa
;   move 10 to codigo-tab-fol
;   search FIRST for codigo-tab-fol in TAB-FOL-F
;   read TAB-FOL-F
;   move bolini to folio-tab-fol
;   rewrite REG-TAB-FOL
;
;   exit.



;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                               MENU VENDEDOR                                ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
MENU-VENDEDOR.
;-------------

    move ' Menu Principal' to proc-ws.
    message 1,  '****************'.
    message 33, '                '.
    examine punt-mp
            when 0 move 6 TO punt-mp        loop.
            when 1 message 17, 'ATENCION CLIENTE'.
            when 2 message 17, '      CAJA      '.
            when 3 message 17, ' VARIOS EN RUTA '.
            when 4 message 17, ' EVENTUALIDADES '.
            when 5 message 17, '   DESCARGAS    '.
            when 6 message 17, '  FIN DEL DIA   '.
            when 7 move 1 TO punt-mp        loop.
    message 49, '****************'.
    perform lee-tecla-fast-action
            when ENTER perform MENU-PROCESO.
            when FAR   add 1 to punt-mp.
            when BAR   subtract 1 from punt-mp.
            when KEND  perform SELECCIONA-MENU exit.
    loop.

MENU-PROCESO.
;------------
    PERFORM LeeHoraFecha
    examine punt-mp
            when 1 perform MENU-VENTA.
            when 2 perform CAJA-VENTA.
            when 3 perform VARIOS-VENTA.
            when 4 perform DESCARGAS-ALM.
            when 5 perform DESCARGAS.
            when 6 perform FIN-DIA.
    clear
    exit.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                CAJA DE MANEJO DE DINERO                 ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
CAJA-VENTA.
;----------
    move '     Caja      ' to proc-ws.
    message 1,  '****************'.
    examine punt-mp
            when 0 move 2 TO punt-mp        loop.
            when 1 message 17, ' Caja  Inicial                  '.
            when 2 message 17, '   Descargas       de  Dinero   '.
            when 3 move 1 TO punt-mp        loop.
    message 49, '****************'.
    perform lee-tecla-fast-action
            when ENTER perform CAJA-VENTA-001.
            when FAR   add 1 to punt-mp.
            when BAR   subtract 1 from punt-mp.
            when KEND  exit.
    loop.

CAJA-VENTA-001.
;--------------
    perform Caj_Ini
    PERFORM LeeHoraFecha
    examine punt-mp
            when 1 move 0 to caja-ini-ws
                   perform MUEVE-CAJA-INI
                   if caja-ini-ws <> 0
                      perform MUESTRA-CAJA-INI
                           when ER exit.
                   end-if
                   perform CAJA-INICIAL.
            when 2 perform Entrega.
    clear
    exit.

MUEVE-CAJA-INI.
;--------------
    perform OPENUP-ARC-CAR-F
    seek FIRST in ARC-CAR-F
      when EOF exit.
    read ARC-CAR-F
    move caja-arc-car to caja-ini-ws
    exit.

MUESTRA-CAJA-INI.
;----------------
    show CAJA-INI-SD
    perform lee-tecla-action
        when ENTER exit.
        when KEND  move ER to last-key exit.
    loop.

CAJA-INICIAL.
;------------
    move 0 to caja-ini-ws.
    accept CAJA-INI-SD
        when OK if caja-ini-ws = 0 loop.
                   perform CONFIRMA-CAJA
                        when ER exit.
                exit.
        when KEND exit.
    loop.

CONFIRMA-CAJA.
;-------------
    message  1, '****************'.
    message 17, 'Confirma Caja..?'.
    message 33, '    Yes / No    '.
    message 49, '****************'.
    perform lee-tecla-action
             when NO    exit.
             when ENTER perform Caj_Ini
                        move caja-ini-ws  to caja-arc-car
                        move caja-ini-ws  to cini-arc-car
                        rewrite REG-ARC-CAR
                        exit.
    loop.


Caj_Ini.
;-------
perform OPENREF-ARC-CAR-F
        when ER perform Caj_Crt loop.
seek FIRST in ARC-CAR-F
        when EOF perform Caj_Crt loop.
read ARC-CAR-F
exit.

Caj_Crt.
;-------
perform OPENUP-ARC-CAR-F
move nCodVd        to c-ve-arc-car
move dia-normal-ws to dia-arc-car
move mes-normal-ws to mes-arc-car
move ano-normal-ws to ano-arc-car
move 0             to kilo-arc-car
move 0             to caja-arc-car
move 0             to cini-arc-car
move 0             to cob-arc-car
move 0             to ade-arc-car
move 0             to cre-arc-car
move 0             to con-arc-car
move 0             to cli-vis-car
move 0             to cli-con-car
move 0             to cli-ate-car
move 0             to cli-can-car
move x'0D'         to cr-arc-car
move x'0A'         to lf-arc-car
write REG-ARC-CAR
exit.

Entrega.
;-------
    move 0 to nCntDin
    accept DESC-DINE1-SD
        when OK   if nCntDin > 0
                     perform GS-VERIFICA-CAJA
                             when ER loop.
                     move 0 to dinero-saca-ws
                     perform Graba_Entrega      ; Graba entrega de Efectivo.
                          when ER exit.
                  end-if
                  perform Pide_Cheque        ; Pide el Cheque
                       when ER exit.
                  perform Confirma_Entrega
                       when ER loop.
                  perform Graba_Entrega_Total
                       when ER exit.

                  exit
          end-when
        when KEND exit.
    loop.

Pide_Cheque.
;-----------
    message  1, '   Entrega de   '.
    message 17, '  Cheques ...?  '.
    message 33, '                '.
    message 49, '    Yes / No    '.
    perform lee-tecla-action
             when NO    exit.
             when ENTER perform DESC-DINERO-3
                             when ER loop.
               end-when
    loop.

DESC-DINERO-3.
;-------------
    move ' ' to cSerieBanco.
    accept SERIE-BCO-SD
        when KEND move ER to last-key exit.
        when OK if cSerieBanco = ' ' loop.
                perform DESC-DINERO-4
                     when ER exit.
                exit.
    loop.

DESC-DINERO-4.
;-------------
    move 0 to nBanco         .
    perform OPENREF-ARC-CHE-F.
         when ER exit.
    seek FIRST in ARC-CHE-F
      when EOF exit.
    search FIRST for cSerieBanco in ARC-CHE-F offset serie-arc-che
        when ER beep
                message 49, 'No Existe Cheque'
                wait 1 seconds.
        when OK read ARC-CHE-F
                move c-bco-arc-che to nBanco
                move monto-arc-che to var5-ws
                show MONTO-CHEQ-SD
                wait 1 second

                perform OPENUP-PRIN-05-F
                move ws-02-factura   to p-05-documento
                move cSerieBanco     to p-05-serie
                move nBanco          to p-05-codigo
                move var5-ws         to p-05-monto
                write REG-PRINT-05

                add  var5-ws         to dinero-saca-ws
        end-when
    exit.


Confirma_Entrega.
;----------------
    message  1, '    Confirma    '.
    message 17, 'Ingreso de Datos'.
    message 33, '                '.
    message 49, '    Yes / No    '.
    perform lee-tecla-action
             when NO    perform OPENUP-PRIN-04-F
                        erase PRIN-04-F
                        perform OPENUP-PRIN-05-F
                        erase PRIN-05-F
                        move ER to last-key exit.

             when ENTER move OK to last-key  exit.
    loop.

Graba_Entrega_Total.
;-------------------
    move '08' to codigo-tab-fol
    perform LEE-FOLIO
    add 1  to folio-tab-fol
    rewrite REG-TAB-FOL.

    perform OPENREF-ARC-CAR-F.
         when ER exit.
    seek FIRST in ARC-CAR-F.
      when EOF exit.
    read ARC-CAR-F.

    subtract dinero-saca-ws from caja-arc-car
    rewrite REG-ARC-CAR.
    perform IMPRE-DINERO-SACA
         when ER exit.
    exit.

Graba_Entrega.
;-------------
    perform OPENUP-PRIN-04-F.
    move nNroDoc       to p-04-documento.
    move nCodVd        to p-04-cod-vende.
    move nCntDin       to p-04-efectivo.
    add  nCntDin       to dinero-saca-ws.
    write REG-PRINT-04.
    exit.

DscDinero.
;---------
subtract nCntDin from caja-arc-car
rewrite REG-ARC-CAR
exit.


IMPRE-DINERO-SACA.
;-----------------
    perform SETEA-IMPRESORA
            when ER exit.
            when KEND move OK to last-key exit.
    clear
    message 1, '* IMPRIMIENDO *'.
    perform OPENREF-PRIN-04-F
         when ER exit.
    next.

IMP-DINE-001.
    build CAB-DINE-000.  print.                 ; Imprime Titulos
    build CABECERA-01.   print.
    build CABECERA-02.   print.
    next.

IMP-DINE-002.
    read PRIN-04-F.
    move p-04-cod-vende to CAMPO50.
    move p-04-documento to CAMPO51.
    move p-04-efectivo  to CAMPO52.
    build CAB-DINE-001.  print.
    seek NEXT in PRIN-04-F.
      when EOF perform OPENREF-PRIN-05-F
                    when ER perform IMP-DINE-004 exit.
               next.
    loop.

IMP-DINE-003.
    read PRIN-05-F.
    move p-05-serie  to CAMPO53.
    move p-05-codigo to CAMPO54.
    move p-05-monto  to CAMPO55.
    build DET-DINE-001.  print.
    seek NEXT in PRIN-05-F.
      when EOF next.
    loop.

IMP-DINE-004.
    move dinero-saca-ws to CAMPO56.
    build DET-DINE-002.  print.
    build FIN-LISTADO.   print.
    disconnect.                                 ; Termino Comunicaciones IP-40
    perform OPENUP-PRIN-04-F.
    erase PRIN-04-F.
    perform OPENUP-PRIN-05-F.
    erase PRIN-05-F.
    exit.

 ;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
 ;ณ DESCARGAS                                             ณ
 ;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

DESCARGAS.
;---------
move '       ' to cNroDoc
move 0         to nCntPrd
move ''        to cCodPrd
move 0         to nNroDoc

perform OPENREF-MAE-PRO-F
        when ER exit.
seek first in MAE-PRO-F
     when EOF exit.
     read MAE-PRO-F
     perform DS-MUEVE-FOLIO.
     perform DS-RECORRE-PRODUCTO.
             when OK exit.
   when KEND exit.
exit.

DS-MUEVE-FOLIO.
;--------------
    move 9 to ws-cod-folio              ; 09 descargas en ruta.
    move '09' to codigo-tab-fol
    perform LEE-FOLIO
    move folio-tab-fol to nNroDoc
    exit.

DS-RECORRE-PRODUCTO.
;-------------------
 move 0              to  nStock
 move codigo-mae-pro to  cCodPrd      ; Para la pantalla
 move nround-mae-pro to  nNroUnd      ; Para la pantalla
 show CODIGO-PRODUCTO-SD
 show MUESTRA-DESC-SD

    perform Ds_Verifica_item
            when OK READ PRIN-11-F
                    move p-11-cnt-prod to nCntPrd
                    show CANT-PRODUCTO-SD.
            when ER perform  VERIFICA-STK             ; Muestra Stock.
                             when ER exit.
            end-when

 action
 when DAR   perform AVANZA-PRODUCTO.
 when UAR   perform RETROC-PRODUCTO.
 when ENTER if nStock = 0  loop.
            perform Ds_Borra_Item
            perform DS-ACEPTA-CANTIDAD
            perform DS-CONFIRMA
                    when ER exit.
            perform DS-GRABA-DESCARGA
            perform AVANZA-PRODUCTO.

 when KEND  perform Ds_Ver_Doc
                    when ER exit.
            perform DS-CONFIRMA-DESCARGA
                    when ER loop.
                    when OK perform DS-IMPRIMIR
                            perform Ds_Graba
                            perform DS-ACTUALIZA-FOLIO
                            exit.
 end-when
 when KELIM perform Ds_Borra_Item.
 loop.

Ds_Borra_Item.
;------------
perform Ds_Verifica_item
   when OK DELETE RECORD FROM PRIN-11-F.
exit.

Ds_Graba.
;--------
perform OPENREF-PRIN-11-F
search first for nNroDoc in PRIN-11-F
       when EOF exit.
       when ER exit.
       when OK read PRIN-11-F.

 perform OPENUP-DESCARGA-F
 move p-11-cod-prod to  CodPrd-des
 move nCntPrd       to  CntPrd-des
 move nNroDoc       to  NroDoc-des
 move nRuta         to  Ruta-des
 move nFecha        to  fecha-des
 move nHora         to  hora-des
 move x'0D'         to  cr-des
 move x'0A'         to  lf-des
 write REG-DESCARGA

move p-11-cod-prod to  cCodPrd
move p-11-cnt-prod to  nCntPrd
move p-11-und-prod to  nNroUnd
perform Iv_Sub_Producto
perform Ds_Add_Inv
perform Ds_Borra
loop.

Ds_Add_Inv.
;-------------
perform OPENREF-ARC-INV
search FIRST for cCodPrd in ARC-INV-F
       when OK  read ARC-INV-F
                move dsc-arc-inv     to nNumero1               ;Aumenta Cajas Vendidas Crdito
                move nCntPrd         to nNumero2
                perform Iv_Suma_Enteros
                move nNumero1        to dsc-arc-inv
                rewrite REG-ARC-INV
       end-when
exit.



Ds_Borra.
;--------
perform OPENUP-PRIN-11-F
search first for nNroDoc in PRIN-11-F OFFSET IS p-11-documento
       when OK DELETE RECORD FROM PRIN-11-F.
exit.


Ds_Borra_todo.
;-------------
perform Ds_Borra
       when OK loop.
exit.



Ds_Verifica_Item.
;----------------
perform OPENUP-PRIN-11-F
move nNroDoc       to Documento
move cCodPrd       to Producto
search first for Clave_Envases in PRIN-11-F
       when EOF move ER  to last-key.
exit.

Ds_Ver_Doc.
;----------
perform OPENUP-PRIN-11-F
search FIRST for nNroDoc in PRIN-11-F
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.


DS-ACEPTA-CANTIDAD.
;------------------
move 0      to nCntPrd
accept CANTIDAD-SD
       when KEND exit.
       when ER  loop.
       when OK  if nCntPrd = 0  exit.
                   move nCntPrd to nCantidad
                   perform Iv_Normal_Entero
                   move nCantidad to nCntPrd
                if nCntPrd > nStock loop.
       end-when

exit.

DS-ACTUALIZA-FOLIO.
;-------------------
    move 9 to ws-cod-folio              ; 9
    search FIRST for ws-cod-folio in TAB-FOL-F
    add 1  to folio-tab-fol
    rewrite REG-TAB-FOL
    exit.

DS-CONFIRMA-DESCARGA.
;-------------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '    Descarga    '.
    message 49, '    Yes / No    '.
    action

    when ENTER  move OK to last-key exit.
    when NO     move ER to last-key exit.
    loop.

DS-CONFIRMA.
;-------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '    Producto    '.
    message 49, '    Yes / No    '.
    action
             when ENTER  move OK to last-key exit.
             when KEND   move ER to last-key exit.
             when NO     move ER to last-key exit.
    loop.

DS-GRABA-DESCARGA.
;-----------------
perform  OPENUP-PRIN-11-F
move nNroDoc         to   p-11-documento
move cCodPrd         to   p-11-cod-prod
move nCntPrd         to   p-11-cnt-prod
move nNroUnd         to   p-11-und-prod
move descri-mae-pro  to   p-11-dsc-prod
write REG-PRINT-11
exit.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ             REPORTE    DESCARGA DE  ENVASES             ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

DS-IMPRIMIR.
;-----------
    perform SETEA-IMPRESORA
            when ER exit.
            when KEND move OK to last-key exit.
    clear.
    message 1, '* IMPRIMIENDO *'.
    next.

DS-IMPRIMIR-DESCARGA .
;---------------------
    search FIRST for nNroDoc in PRIN-11-F
         when ER prompt 1 'Error buscando en spool'  wait 1 second exit.
    read  PRIN-11-F

    build CAB-FACT-000     print
    build CAB-DS-001       print
    build CAB-DS-002       print
    perform DS-IMPRIMIR-SIGUIENTE
    build CAB-DS-004       print
    build FIN-LISTADO      print
    disconnect                                  ; Termino Comunicaciones IP-40
    exit.


DS-IMPRIMIR-SIGUIENTE.
;---------------------
if p-11-documento =  nNroDoc
   build CAB-DS-003    print
   seek NEXT in PRIN-11-F
     when EOF exit.
   read PRIN-11-F
   loop
end-if
exit.


;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                       FIN DEL DIA                       ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
FIN-DIA.
;-------
   perform Caj_Ini
           when ER exit.

   perform OPENREF-TAB-FOL-F
   perform REPORTE-FIN-DIA
   exit.

FIN-DIA-005.
;-----------
    perform OPENREF-ARC-INV
         when ER exit.
    seek FIRST in ARC-INV-F
         when EOF exit.
    perform OPENREF-MAE-PRO-F
    seek FIRST in MAE-PRO-F
         when EOF exit.

    exit.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                     REPORTE FIN DIA                     ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
REPORTE-FIN-DIA.
;---------------
    perform SETEA-IMPRESORA
            when ER exit.
            when KEND move OK to last-key exit.
    clear.
    message 1, '* IMPRIMIENDO *'.
    next.

IMP-FIN-DIA-001.
;----------------

    move 20000             to user-tmout
    build CABECERA-01       print               ; Imprime CABECERAS
    build CABECERA-02       print
    build CAB-FIN-DIA-001   print
    perform FIN-DIA-005
            when OK build INVENTARIO    print
                    seek FIRST IN ARC-INV-F
                    perform IMP-INVENTARIO.
    perform IMP-FIN-DIA-004
    exit.


IMP-INVENTARIO.                                  ; Imprime Inventario
;--------------
    read ARC-INV-F
    when EOF next.
    move codigo-arc-inv to busca-pro-ws
    perform BUSCA-COD-PROD
    build Inv_Fin_Dia     print
    if vco-arc-inv > 0
       build Inv_Fin_Dia-1   print.

    if vcr-arc-inv > 0
       build Inv_Fin_Dia-2   print.

    if pen-arc-inv > 0
       build Inv_Fin_Dia-3   print.

    if den-arc-inv > 0
       build Inv_Fin_Dia-4   print.

    if cso-arc-inv > 0
       build Inv_Fin_Dia-5   print.

    if csr-arc-inv > 0
       build Inv_Fin_Dia-6   print.

    if recarga-inv > 0
       build Inv_Fin_Dia-7   print.

    if obs-arc-inv > 0
       build Inv_Fin_Dia-8   print.

    if dsc-arc-inv > 0
       build Inv_Fin_Dia-9   print.

    if cjs-arc-inv > 0
       build Inv_Fin_Dia-10  print.

    if rts-arc-inv > 0
       build Inv_Fin_Dia-11  print.

    build Inv_Fin_Dia-12  print

    seek NEXT in ARC-INV-F
      when EOF exit.
    loop.

IMP-FIN-DIA-004.                                  ; Imprime Cabecera
;---------------                                  ; de Folios.
build CAB-FIN-DIA-FOL print
next.

IMP-FIN-DIA-005.
;---------------
read TAB-FOL-F
move folio-tab-fol to nEntero
move folio-tab-fol to nNroDoc
subtract 1       from nNroDoc
subtract folio-tab-ini from nEntero
if nEntero > 0
   build DET-FIN-DIA-FOL   print.
seek NEXT in TAB-FOL-F
     when EOF next.
loop.

IMP-FIN-DIA-006.
;---------------
    perform OPENREF-ARC-CAR-F
         when ER exit.

    seek FIRST in ARC-CAR-F
      when OK read ARC-CAR-F
              build DET-FIN-DIA-002 print
              build DET-FIN-DIA-003 print.

    build FIN-LISTADO    print
    disconnect                                  ; Termino Comunicaciones IP-40
    exit.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                       MENU-VENTA                        ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
MENU-VENTA.
;----------
    move 1 to punt-ped.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                      CLIENTE RUTA                       ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
RUTA-001.
;--------
    perform OPENREF-IDENTIF-F
         when ER exit.
    seek FIRST in IDENTIFICA-F
            when EOF message 1,  '****************'
                     message 17, 'Archivo Identif.'
                     message 33, '     Vacio      '
                     message 49, '****************'
                     wait 1 second
                     exit.
    read IDENTIFICA-F
    move cod-arc-ide to nCodVd

    move dow-normal-ws  to nDiaRuta
    perform Busca_Rutas
            when ER exit.

    perform OPENREF-MAE-CLIE
         when ER exit.
    perform RUTA-003
    move OK to last-key
    exit.

Busca_Rutas.
;-----------
    move 0              to cod-ruta1-ws
    move 0              to cod-ruta2-ws
    move 0              to cod-ruta3-ws
    move 0              to cod-ruta4-ws
    move 0              to nCantidad
    move 1              to menu-ruta
    perform OPENREF-TAB-RUT.
         when ER exit.

    seek FIRST in TAB-RUT-F
            when EOF message 1,  '****************'
                     message 17, ' Archivo Tabla  '
                     message 33, '   Ruta Vacio   '
                     message 49, '****************'
                     wait 1 second
                     exit.
            when  OK read TAB-RUT-F.

    show COD-VENDE-SD
    show NOM-VENDEDOR-SD
    show FECHA-HOY-SD
    perform ELIGE-RUTAS
    if  cod-ruta1-ws = 0 and cod-ruta2-ws = 0 and cod-ruta3-ws = 0 and cod-ruta4-ws = 0
        perform M-ERROR
        perform Acepta_DiaRuta loop.

    show RUTA-HOY-SD
    move 1 to punt-ruta
    perform SELECCIONA-RUTA
         when ER exit.
         when KMOD perform Acepta_DiaRuta loop.
exit.

ELIGE-RUTAS.
;-------------
perform BUSCA-RUTA                         ; Busca la primera ruta
    when ER perform Siguiente_ruta
                    when EOF exit.
    end-when
    when Ok add 1 to nCantidad.
next.

ELIGE-RUTAS1.
;-------------
if nCantidad = 1
   move codigo-tab-rut to cod-ruta1-ws
   move codigo-tab-rut to cod-ruta2-ws
end-if
if nCantidad = 2
   move codigo-tab-rut to cod-ruta2-ws
end-if
if nCantidad = 3
   move codigo-tab-rut to cod-ruta3-ws
   move codigo-tab-rut to cod-ruta4-ws
end-if
if nCantidad = 4
   move codigo-tab-rut to cod-ruta4-ws
end-if

perform Siguiente_ruta
       when EOF exit.
loop.

Siguiente_ruta.
;--------------
seek NEXT in TAB-RUT-F
     when EOF exit.
read TAB-RUT-F
if codigo-tab-rut = 0
   loop.
perform BUSCA-RUTA                         ; Busca la primera ruta
    when ER loop.
add 1 to nCantidad
exit.

BUSCA-RUTA.
;----------
   ;if dow-normal-ws = 0

    move OK to last-key
    if nDiaRuta = 1
       if lun-tab-rut = 1
          exit.
    end-if
    if nDiaRuta = 2
       if mar-tab-rut = 1
          exit.
    end-if
    if nDiaRuta = 3
       if mie-tab-rut = 1
          exit.
    end-if
    if nDiaRuta = 4
       if jue-tab-rut = 1
          exit.
    end-if
    if nDiaRuta = 5
       if vie-tab-rut = 1
          exit.
    end-if
    if nDiaRuta = 6
       if sab-tab-rut = 1
          exit.
    end-if
    move ER to last-key
    exit.

M-ERROR.
;-------
    message 1,  '****************'.
    message 17, ' No Existe Ruta '.
    message 33, ' para este dia  '.
    message 49, '****************'.
    wait 1 second
    move ER to last-key
    exit.

SELECCIONA-RUTA.
;---------------
    examine punt-ruta
            when 0 if menu-ruta = 2
                      move 1 to menu-ruta
                      SHOW RUTA-HOY-SD.
                   move 2 TO punt-ruta      loop.
            when 1 message 54, '>'
                   message 60, ' '.
            when 2 message 60, '>'
                   message 54, ' '.
            when 3 if menu-ruta = 1 and  cod-ruta3-ws > 0
                      move 2 to menu-ruta
                      SHOW RUTA1-HOY-SD.
                   move 1 TO punt-ruta      loop.

    perform lee-tecla-fast-action
            when ENTER perform SEL-RUTA-01 exit.
            when FAR   add      1 to   punt-ruta.
            when BAR   subtract 1 from punt-ruta.
            when KEND  move ER to last-key, exit.
            when KMOD  exit.
    loop.

Acepta_DiaRuta.
;--------------
move 1 to punt-dia-vis
clear
message  1, '   Elegir dia   '
message 17, '    deseado     '
message 49, '  L M M J V S   '
perform Acepta_Sec_Ruta
exit.


Acepta_Sec_Ruta.
;--------------
    examine punt-dia-vis
       when 0 move 1 to punt-dia-vis loop.
       when 1 accept DIA-1-SD
              if dia-1 not = 'S' and dia-1 not = 'N' loop.
              if dia-1 = 'S'
                 move 1 to nDiaRuta exit.
            end-when
       when 2 accept DIA-2-SD
              if dia-2 not = 'S' and dia-2 not = 'N' loop.
              if dia-2 = 'S'
                 move 2 to nDiaRuta exit.
            end-when
       when 3 accept DIA-3-SD
              if dia-3 not = 'S' and dia-3 not = 'N' loop.
              if dia-3 = 'S'
                 move 3 to nDiaRuta exit.
       end-when
       when 4 accept DIA-4-SD
              if dia-4 not = 'S' and dia-4 not = 'N' loop.
              if dia-4 = 'S'
                 move 4 to nDiaRuta exit.
       end-when
       when 5 accept DIA-5-SD
              if dia-5 not = 'S' and dia-5 not = 'N' loop.
              if dia-5 = 'S'
                 move 5 to nDiaRuta exit.
            end-when
       when 6 accept DIA-6-SD
              if dia-6 not = 'S' and dia-6 not = 'N' loop.
              if dia-6 = 'S'
                 move 6 to nDiaRuta exit.
            end-when
       when 7  exit.
    no-op
       when OK    add 1 to punt-dia-vis.
       when BAR   subtract 1 from punt-dia-vis.
       when KEND  move ER to last-key exit.
    loop.


SEL-RUTA-01.
;-----------
    move 0 to nRuta.
    examine punt-ruta
         when 1 if menu-ruta =  1
                   move cod-ruta1-ws to nRuta
                else
                   move cod-ruta3-ws to nRuta.
         end-when
         when 2 if menu-ruta =  1
                   move cod-ruta2-ws to nRuta
                else
                   move cod-ruta4-ws to nRuta.
         end-when
    exit.

RUTA-003.
;--------
move 0   to nCodCli          .
move 0   to ruc-cliente-ws.
move ' ' to digito-ws.
move ' ' to cNomcli          .
search FIRST for nRuta in MAE-CLI-F OFFSET IS ruta-mae-cli
when ER  ALARM
         message 1,  '****************'
         message 17, 'No hay clientes '
         message 33, 'para esta ruta!!'
         message 49, '****************'
         wait 1 second
         exit.

read MAE-CLI-F
perform MUESTRA-DATOS-CLIE
exit.


Acepta_Cliente.
;--------------
    accept CODIGO-CLIENTE-SD
        when OK    if nCodCli           = 0 loop.
                   perform BUSCA-CLIENTE-RUTA
                        when ER loop.
          end-when
        when UAR   perform Cl_Fin.
        when DAR   perform Cl_Inicio.
exit.

MUESTRA-DATOS-CLIE.
;------------------
       move codigo-mae-cli to nCodCli
       move ruc-mae-cli    to cli-rut-ws
       move n-estb-mae-cli to cNomcli
       move direcc-mae-cli to cDircli
       if nPanCli = 0
          show CODIGO-CLIENTE-SD
          perform VERIFICA-VENTA
                  when OK  READ CFA-VTA-F
                           if tipo-cfa-vta = 1
                              message 51, "Co".

                           if tipo-cfa-vta = 2
                              message 51, "Cr".

                           if tipo-cfa-vta = 0
                              message 51, "An".
                  end-when
                  when ER  message 51, "  ".

          perform VERIFICA-COBRANZA
                  when OK  message 53, "C".
                  when ER  message 53, " ".

          perform VERIFICA-PRESTAMO
                  when OK  message 54, "P".
                  when ER  message 54, " ".

          perform VERIFICA-DEVOLUCION
                  when OK  message 55, "D".
                  when ER  message 55, " ".

          perform VERIFICA-CSGDEV
                  when OK  message 57, "Cd".
                  when ER  message 57, "  ".

          perform VERIFICA-CSG
                  when OK  message 59, "Ct".
                  when ER  message 59, "  ".
          ;--------------------------------------------
          perform Verifica_Saldos



       end-if
       if nPanCli = 1
          show DIRE-CLIENTE-SD
          show RUT-SD.

       if nPanCli = 2
          perform Dsc_Sld_Cli.

       perform lee-tecla-fast-action
             when ENTER perform LeeHoraFecha
                        move 0 to swich-venta
                        move 0 to nAtencion
                        perform Graba-Visitado
                        perform Muestra_Mensaje
                        perform ATENCION
                             when ER exit.
                        loop.
             when KNEXT examine nPanCli
                           when 0  move 1 to nPanCli.
                           when 1  move 2 to nPanCli.
                           when 2  move 0 to nPanCli.
             end-when
             when KFIND perform Acepta_cliente.
             when DAR   perform AVANZA-REGISTRO.
             when NO    perform AVANZA-REGISTRO.
             when UAR   perform RETROC-REGISTRO.
             when KEND  exit.
    loop.

Verifica_Saldos.
;--------------
          perform VERIFICA-PENDIENTE
                  when OK  message 61, "C".
                  when ER  message 61, " ".

          perform VERIFICA-ENVASES
                  when OK  message 62, "E".
                  when ER  message 62, " ".

          perform VERIFICA-PEDIDOS
                  when OK  message 63, "P".
                  when ER  message 63, " ".

          perform VERIFICA-CSGPEND
                  when OK  message 64, "Cs".
                  when ER  message 64, "  ".

exit.


Dsc_Sld_Cli.
;-----------
move 0 to nSldTot                 ; Crditos
move 0 to nSldCsg                 ; Consignacion.
move 0 to nSldEnv                 ; Envases
perform Sum_Sld_Cli
perform Sum_Env_Cli
perform Sum_Csg_Cli

clear
message 1,  '   Saldos:      '
SHOW Dsp_Sld-SD
exit.



Sum_Sld_Cli.
;-----------
perform VERIFICA-PENDIENTE
        when ER exit.

Sum_Sld_Cli001.
;--------------
READ  ARC-COB-F
if nCodCli <> codigo-arc-cob
   exit.
add  m-ade-arc-cob  to nSldTot

search NEXT for nCodCli ARC-COB-F
      WHEN EOF exit.
      WHEN ER  exit.
loop.

Sum_Env_Cli.
;-----------
perform VERIFICA-ENVASES
        when ER exit.
Sum_Env_Cli001.
;--------------
READ  DET-ENV-F
if nCodCli <> codigo-det-env
   exit.
add  can-pre-det-env to nSldEnv

;SEEK NEXT in DET-ENV-F
search NEXT for nCodCli DET-ENV-F
      WHEN EOF exit.
      WHEN ER  exit.
loop.


Sum_Csg_Cli.
;-----------
perform VERIFICA-CSGPEND
        when ER exit.
Sum_Csg_Cli001.
;--------------
READ  DET-CSG-F
if nCodCli <> codigo-det-csg
   exit.
add  can-pre-det-csg to nSldCsg

;SEEK NEXT in DET-CSG-F
search NEXT for nCodCli DET-CSG-F
      WHEN EOF exit.
      WHEN ER  exit.
loop.




Cl_Fin.
;------
    seek LAST in MAE-CLI-F.
    read MAE-CLI-F.
    if ruta-mae-cli <> nRuta
       perform RETROC-REGISTRO
            when EOF exit.
    end-if
    exit.

Cl_Inicio.
;---------
    seek FIRST in MAE-CLI-F.
    read MAE-CLI-F.
    if ruta-mae-cli <> nRuta
       perform AVANZA-REGISTRO
            when EOF exit.
    end-if
    exit.

Muestra_Mensaje.
;---------------
perform OPENREF-MENSAJE-CLIENTE
        when OK search FIRST for nCodCli in MENSAJE-CLI-F
                    when KEXIT exit.
                    when KEND  exit.
                    when OK    read MENSAJE-CLI-F
                               message  1, '****************'
                               message 49, '****************'
                               show MENSAJE-CLI-SD
                               action.
             end-when
exit.


Graba-Visitado.
;--------------
perform Caj_Ini
add 1 to cli-vis-car
perform Vs_VerCli
        when ER  add 1 to cli-can-car.
        when EOF add 1 to cli-can-car.
rewrite REG-ARC-CAR

perform OPENUP-ARC-ATE-F
move nCodCli            to codigo-arc-ate
move 0                  to venta-arc-ate
move 0                  to estado-arc-ate
move 0                  to motivo-arc-ate
move nFecha             to fecha-arc-ate
move nHora              to HoraI-arc-ate
move nHora              to HoraF-arc-ate
move nRuta              to Ruta-arc-ate
move nCodVd             to Vend-arc-ate
move x'0D'              to cr-arc-ate
move x'0A'              to lf-arc-ate
write REG-ARC-ATE
exit.


Vs_VerCli.
;---------
move 'ATENDI'     to nombre-fn
move nCodVd       to codigo-fn
move ws-file-name to file-name
open REF ARC-ATE-F
     when ER perform OPENUP-ARC-ATE-F.
search FIRST for nCodCli  in ARC-ATE-F
       when EOF move ER to last-key.
exit.

VERIFICA-COBRANZA.
;-----------------
move 'DCO-XX'       to nombre-fn
move nCodVd         to codigo-fn
move ws-file-name to file-name
open REF DCO-PAG-F
     when ER perform OPENUP-DCO-PAG-F.
search FIRST for nCodCli  in DCO-PAG-F
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.

VERIFICA-CSGDEV.
;---------------
move 'DEVCSG'       to nombre-fn.
move nCodVd       to codigo-fn.
move ws-file-name to file-name.
open REF DEV-CSG-F
     when ER perform OPENUP-DEV-CSG-F.
search FIRST for nCodCli  in DEV-CSG-F
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.


VERIFICA-CSG.
;------------
move 'CONSIG'       to nombre-fn.
move nCodVd       to codigo-fn.
move ws-file-name to file-name.
open REF CAB-CSG-F
        when ER perform OPENUP-CAB-CSG-F.
search FIRST for nCodCli  in CAB-CSG-F
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.

VERIFICA-VENTA.
;--------------
move 'CFA-XX'       to nombre-fn.
move nCodVd       to codigo-fn.
move ws-file-name to file-name.
open REF CFA-VTA-F
     when ER perform OPENUP-CFA-VTA-F.
search FIRST for nCodCli  in CFA-VTA-F
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.

VERIFICA-PRESTAMO.
;-------------------
move 'PRESTA'       to nombre-fn.
move nCodVd       to codigo-fn.
move ws-file-name to file-name.
open UPDATE CAB-PRE-F
     when ER perform OPENUP-CAB-PRE-F.
search FIRST for nCodCli  in CAB-PRE-F
     when EOF move ER to last-key exit.
     when ER exit.
     when OK exit.
    exit.

VERIFICA-DEVOLUCION.
;-------------------
move 'DEVENV'     to nombre-fn.
move nCodVd       to codigo-fn.
move ws-file-name to file-name.
open REF DEV-ENV-F
     when ER perform OPENUP-DEV-ENV-F.
search FIRST for nCodCli  in DEV-ENV-F
     when EOF move ER to last-key exit.
     when ER exit.
     when OK exit.
    exit.

VERIFICA-PENDIENTE.
;-----------------
move 'ACO-XX'     to nombre-fn
move nCodVd       to codigo-fn
move ws-file-name to file-name
open REF ARC-COB-F
     when ER perform OPENUP-ARC-COB-F.
search FIRST for nCodCli  in ARC-COB-F
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.

VERIFICA-ENVASES.
;----------------
move 'ENV-XX'       to nombre-fn.
move nCodVd       to codigo-fn.
move ws-file-name to file-name.
open REF DET-ENV-F
     when ER perform OPENUP-DET-ENV-F.
search FIRST for nCodCli  in DET-ENV-F
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.

VERIFICA-PEDIDOS.
;----------------
move 'PED-XX'       to nombre-fn.
move nCodVd       to codigo-fn.
move ws-file-name to file-name.
open REF PEDIDOS-F
     when ER perform OPENUP-PEDIDOS-F.
search FIRST for nCodCli  in PEDIDOS-F
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.



VERIFICA-CSGPEND.
;----------------
move 'CSGPEN'       to nombre-fn.
move nCodVd       to codigo-fn.
move ws-file-name to file-name.
open REF DET-CSG-F
     when ER perform OPENUP-DET-CSG-F.
search FIRST for nCodCli  in DET-CSG-F
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.

AVANZA-REGISTRO.
;---------------
    seek NEXT in MAE-CLI-F
         when EOF message 49, '- Fin de Ruta  -'
                  beep
                  wait 1 second
                  perform Cl_Fin
                  exit.
         when OK  read MAE-CLI-F
                  if ruta-mae-cli = nRuta
                  exit.
           end-when
    loop.

RETROC-REGISTRO.
;---------------
    seek PRIOR in MAE-CLI-F
         when EOF message 49, '-Inicio de Ruta-'
                  beep
                  wait 1 second
                  perform Cl_Inicio
                  exit.
         when OK  read MAE-CLI-F
                  if ruta-mae-cli = nRuta
                  exit.
           end-when
    loop.

BUSCA-CLIENTE-RUTA.
;------------------
    search FIRST for nCodCli           in MAE-CLI-F
        when ER beep
                message 1,  '****************'
                message 17, '    Cliente     '
                message 33, '  No Existe !!  '
                message 49, '****************'
                wait 1 second
                perform CHEQUEA-CREACION
                     when ER exit.
                exit.

        when OK read MAE-CLI-F
                if ruta-mae-cli <> nRuta
                   message 1,  '****************'
                   message 17, 'Cliente de Otra '
                   message 33, '      Ruta      '
                   message 49, '****************'
                   wait 1 second
                   exit
                 end-if
          end-when
    exit.

CHEQUEA-CREACION.
;----------------
    message  1, '****************'.
    message 17, 'Crea Cliente...?'.
    message 33, '    Yes / No    '.
    message 49, '****************'.
    perform lee-tecla-action
             when ENTER perform NUEVO
                             when ER exit.
                        move 0 to swich-venta
                        move 0 to nAtencion
                        perform Graba-Visitado
                        perform ATENCION
                             when ER exit.
                        if swich-venta = 1 loop.
                        exit.
             when NO    move ER to last-key exit.
    loop.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                      CLIENTE NUEVO                      ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
NUEVO.
;-----
    clear
    move 1 to punt-ingreso.
    perform ACCEP-DATOS-PERSONALES
         when ER exit.
    perform CONFIRMA-NUEVO
         when ER exit.
    exit.

ACCEP-DATOS-PERSONALES.
;----------------------
    examine punt-ingreso
         when 0 move 1 to punt-ingreso loop.
         when 1 move 'N' to shift-char
                move ' ' to nombre-propie-ws
                accept NOMBRE-PROPIE-SD
                    when OK move OK to last-key.
           end-when
         when 2 move 'N' to shift-char
                move ' ' to nombre-estable-ws
                accept NOMBRE-ESTABLE-SD
                    when OK move OK to last-key.
           end-when
         when 3 move 'D' to shift-char
                move ' ' to dire-estable-ws
                accept DIRE-ESTABLE-SD
                    when OK move OK to last-key.
           end-when
         when 4 move 0 to tele-estable-ws
                accept TELE-ESTABLE-SD
                    when OK move OK to last-key.
           end-when
         when 5 perform ACCEP-RUT.
         when 6 perform ACEPTA-CODRAMO.
         when 7 perform Acepta-Secuencia-Vis.
         when 8 move '@' to shift-char, exit.

    no-op.
         when OK   add 1 to punt-ingreso.
         when FAR  subtract 1 from punt-ingreso.
         when KEND move ER to last-key exit.
    loop.

ACEPTA-SECUENCIA-VIS.
;--------------------
move 1 to punt-dia-vis
clear
message  1, ' Dias de visita '
message 17, '    Deseada     '
message 49, '  L M M J V S   '
perform ACEPTA-SEC-VIS

exit.



ACEPTA-SEC-VIS.
;--------------
    examine punt-dia-vis
       when 0 move 1 to punt-dia-vis loop.
       when 1 accept DIA-1-SD
              if dia-1 not = 'S' and dia-1 not = 'N' loop.
            end-when
       when 2 accept DIA-2-SD
              if dia-2 not = 'S' and dia-2 not = 'N' loop.
            end-when
       when 3 accept DIA-3-SD
              if dia-3 not = 'S' and dia-3 not = 'N' loop.
       end-when
       when 4 accept DIA-4-SD
              if dia-4 not = 'S' and dia-4 not = 'N' loop.
       end-when
       when 5 accept DIA-5-SD
              if dia-5 not = 'S' and dia-5 not = 'N' loop.
            end-when
       when 6 accept DIA-6-SD
              if dia-6 not = 'S' and dia-6 not = 'N' loop.
            end-when
       when 7 perform CONFIRMA-SECUENCIA-VIS
                  when ENTER move OK to last-key exit.
                  when YES   move OK to last-key exit.
                  when NO    move 1 to punt-dia-vis
                             clear
                             message  1, ' Dias de visita '
                             message 17, '    Deseada     '
                             message 49, '  L M M J V S   '
                             loop.
            end-when
    no-op
       when OK    add 1 to punt-dia-vis.
       when BAR   subtract 1 from punt-dia-vis.
    loop.

CONFIRMA-SECUENCIA-VIS.
;----------------------
    clear.
    message  1, '****************'.
    message 17, '  Secuencia OK  '.
    message 33, '    Yes / No    '.
    message 49, '****************'.
    action.
       when ENTER exit.
       when YES   exit.
       when NO    exit.
    loop.

ACEPTA-CODRAMO.
;--------------
    perform OPENREF-RAMOS
            when ER exit.
    seek FIRST in RAMOS-F
            when EOF message 1,  '****************'
                     message 17, ' Archivo  Ramos '
                     message 33, '     Vacio      '
                     message 49, '****************'
                     wait 1 second
                     exit.
    read RAMOS-F.

ACEPTA-CODRAMO-001.
;------------------

if nTipNeg <> 0
   search FIRST for nTipNeg in RAMOS-F
          when ER seek FIRST in RAMOS-F.
          when OK read RAMOS-F.
   perform  REC-RAMOS
            when OK exit.
end-if


ACEPTA-CODR-002.
;------------------
    accept COD-RAMO-SD
         when OK    move ER to last-key
                    search FIRST for nTipNeg in RAMOS-F
                         when OK read RAMOS-F
                                 perform REC-RAMOS
                                       when OK exit.
                                 exit.
              end-when
         when UAR   seek LAST in RAMOS-F
                    read RAMOS-F
                    perform REC-RAMOS
                            when OK exit.
                    exit.
         when DAR   seek FIRST in RAMOS-F
                    read RAMOS-F
                    perform REC-RAMOS
                            when OK exit.
                    exit.
    loop.

REC-RAMOS.
;---------
    move ra-codigo to nTipNeg.
    show COD-RAMO-SD.
    show DESC-RAMO-SD.
    fast-action
         when ENTER move OK to last-key exit.
         when UAR   seek PRIOR in RAMOS-F
                        when EOF beep
                                 message 49, ' Inicio Ramos   '
                                 wait 1 second
                                 seek FIRST in RAMOS-F.
                    read RAMOS-F.
         when DAR   seek NEXT in RAMOS-F
                        when EOF beep
                                 message 49, '  Fin Ramos     '
                                 wait 1 second
                                 seek LAST in RAMOS-F.
                    read RAMOS-F.
    loop.



CONFIRMA-NUEVO.
;--------------
    message  1, '****************'.
    message 17, 'Confirma Cliente'.
    message 33, '    Yes / No    '.
    message 49, '****************'.
    perform lee-tecla-action
             when ENTER ;perform respalda-punt-cliente
                        perform OPENUP-ARC-NUE-F
                        move cli-rut1-ws       to serie-arc-nue
                        move cli-dig-ws        to dig-arc-nue
                        move nCodCli           to codigo-arc-nue
                        move nombre-propie-ws  to n-prop-arc-nue
                        move nombre-estable-ws to n-estb-arc-nue
                        move dire-estable-ws   to direcc-arc-nue
                        move tele-estable-ws   to telefo-arc-nue
                        move nRuta             to ruta-arc-nue
                        move nTipNeg           to nego-arc-nue
                        move cDias_Visita      to frec-arc-nue
                        move x'0D'             to cr-arc-nue
                        move x'0A'             to lf-arc-nue
                        write REG-ARC-NUE
      ;                 perform recupera-punt-cliente
                        perform agrega-cliente
                        exit.
             when NO    move ER to last-key, exit.
    loop.

RESPALDA-PUNT-CLIENTE.
;--------------------
   MOVE MAE-CLI-F:BEGIN-POINTER  TO PUNT-INICIO                ;direccion inicial
   MOVE MAE-CLI-F:REVIEW-POINTER TO PUNT-FIN                   ;direccion actual
   MOVE MAE-CLI-F:RECORD-LENGTH  TO LARGO-REGISTRO             ;tamaคo de registro
   PERFORM CALC-LARGO-FILE                                     ;tamaคo=dir.act. - dir.ini.
   IF LARGO-ARCHIVO > 0
      DIVIDE LARGO-REGISTRO INTO LARGO-ARCHIVO.
   MOVE LARGO-ARCHIVO TO NRO-REG
   ADD 1 TO NRO-REG
EXIT.

CALC-LARGO-FILE.
;---------------
   MOVE PUNT-FIN TO LARGO-ARCHIVO
   SUBTRACT PUNT-INICIO FROM LARGO-ARCHIVO
EXIT.

RECUPERA-PUNT-CLIENTE.
;---------------------
   SEEK NRO-REG IN MAE-CLI-F
   WHEN OK READ MAE-CLI-F. ;siempre debe encontrarlo !!!
EXIT.

AGREGA-CLIENTE.
;--------------
   perform OPENUP-MAE-CLIE
   search first for codigo-mae-cli in MAE-CLI-F
   seek NEXT in MAE-CLI-F
   move nCodCli           to codigo-mae-cli
   move ruc-arc-nue       to ruc-mae-cli
   if n-estb-arc-nue1 <> ' '
      move n-estb-arc-nue    to n-estb-mae-cli
   else
      move n-prop-arc-nue    to n-estb-mae-cli
   end-if
 ; move n-prop-arc-nue    to n-prop-mae-cli
   move direcc-arc-nue    to direcc-mae-cli
   move nRuta             to ruta-mae-cli
  ;write   REG-MAE-CLI    ;no actualiza puntero REVIEW-POINTER
   insert  REG-MAE-CLI
  ;seek last in MAE-CLI-F ;actualiza puntero REVIEW-POINTER

   move n-prop-arc-nue to cNomcli
   move direcc-arc-nue to cDircli
   move ruc-arc-nue    to cli-rut-ws
EXIT.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                     VALIDACION RUT                      ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
ACCEP-RUT.
;---------
    accept RUT-SD.
        when ER move OK to last-key exit.
        when KEND  move ER to last-key exit.
      ; when OK accept DIGV-SD
      ;             when KEND  exit.
                    when OK move cli-rut1-ws to rut-1
                            move 2 to ws-con
                            move 8 to ws-tbl
                            move 0 to ws-suma
                            move 0 to resto-aux1
                           ;perform VALIDA-DIGV11
                           ;if cli-dig-ws = ws-dig
                               move OK to last-key
                               exit.
                 ;    end-when
       ;  end-when
    beep
    message 49, '* Ruc Invalido *'
    wait 1 second
    loop.

VALIDA-DIGV11.
;-------------
    if ws-tbl < 1 then
       move ws-suma to ws-resto1
       perform COMPARA
       exit.
    move ws-num(ws-tbl) to ws-num-aux.
    multiply ws-num-aux by ws-con.
    add ws-num-aux      to ws-suma.
    if ws-con = 7
       move 1 to ws-con.
    subtract 1 from ws-tbl.
    add 1 to ws-con.
    loop.

COMPARA.
;-------
    divide 11 into ws-resto1 remainder resto-aux1.
    subtract 11 from resto-aux1.
    if resto-aux1 = 10
       move 'K' to ws-dig
       exit.
    if resto-aux1 = 11
       move 0 to ws-dig
       exit.
    move resto-aux1 to resto-aux.
    move resto-aux  to ws-dig.
    exit.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                      NOTA DE VENTA                      ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
ATENCION.
;----------
    move ' ' to cDias_Visita
    move 1 to punt-ped.

ATENCION-001.
;------------
    move ' Atencion Clie.' to proc-ws.
    message 1,  '****************'.
    examine punt-ped
            when 0 move 5 to punt-ped       loop.
            when 1 message 17 '     Venta      '.
            when 2 message 17 '    Cobranza    '.
            when 3 message 17 '    Envases     '.
            when 4 message 17 '     Canjes     '.
     ;      when 5 message 17 '     Censo      '.
            when 5 message 17 ' Consignaciones '.
            when 6 move 1 to punt-ped       loop.
    message 49, '****************'.
    message 33, '    Yes / No    '.
    perform lee-tecla-fast-action
            when ENTER perform SUB-MENU-VENTA.
            when KEND ;perform VERIFICA-COMPRA
                      ;        when ER perform MOTIVO-NO-VENTA.
                       perform AVANZA-REGISTRO
                       exit.
            end-when
            when NO    add 1 to punt-ped.
            when FAR   add 1 to punt-ped.
            when BAR   subtract 1 from punt-ped.
    loop.

VERIFICA-COMPRA.
;---------------
  perform OPENREF-ARC-ATE-F
  seek LAST in ARC-ATE-F
  read ARC-ATE-F
 if venta-arc-ate = 0
    move ER  to last-key exit.
 if venta-arc-ate = 1
    perform LeeHoraFecha
    move nHora  to HoraF-arc-ate
    rewrite REG-ARC-ATE
    move OK  to last-key exit.
exit.


SUB-MENU-VENTA.
;--------------
    move   0   to    lFlgFcCs
    move   0   to    lFlgIvCs
    examine punt-ped
            when 1 perform FACTURACION.
            when 2 perform COBRANZAS.
            when 3 perform ENVASES.
            when 4 perform CANJES.
           ;when 5 perform CENSO-VENTA.
            when 5 perform CONSIGNACIONES.
    move   0   to    lFlgFcCs
    move   0   to    lFlgIvCs
    exit.

;ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
;บ                         FACTURACION                                       บ
;ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ

FACTURACION.
;-----------
    move 1 to punt-fc
    next.

FC-001.
;-------
    move '  Facturaciขn  ' to proc-ws
    message 1,  '****************'
    examine punt-fc
            when 0 move 2 to punt-fc        loop.
            when 1 message 17 '    Ingreso                    '.
            when 2 message 17 '  Borrar Venta                 '.
            when 3 move 1 to punt-fc      loop.
    message 33, '    Yes / No    '
    perform lee-tecla-fast-action
            when ENTER perform FC-002 exit.
            when KEND  exit.
            when NO    add 1 to punt-fc.
            when FAR   add 1 to punt-fc.
            when BAR   subtract 1 from punt-fc.
    loop.

FC-002.
;---------
    examine punt-fc
            when 1 perform FC_INGRESO.
            when 2 perform FC_ANULA_CAB
                           when ER exit.
                   perform FC_ANULA_DET
                   perform FC_BORRA_PENDIENTE
            end-when
    exit.


FC_ANULA_CAB.
;------------
   perform BOLET-FACT
           when ER exit.
    if punt-bofa = 1
       move 'F'            to cBoFa
    else
       move 'B'            to cBoFa
    end-if
  ;accept FcAnula-SD
   clear
   accept NRO-DOC-SER-SD
   accept NRO-DOC-NRO-SD
        when BAR loop.
        when KEND move ER to last-key exit.

   perform OPENUP-CFA-VTA-F
   search FIRST for cFactura in CFA-VTA-F OFFSET IS nfac-cfa-vta
      when EOF beep exit.
      when ER  beep
               message  1, '****************'
               message 17, ' No existe esta '
               if punt-bofa = 1
                  message 33, '    FACTURA     '
               else
                  message 33, '    BOLETA      '
               end-if
               message 49, '****************'
               wait 1 second
               move ER to last-key loop.

      when OK perform Fc_confirma_Anula
              when ER exit.
              READ CFA-VTA-F
              move tipo-cfa-vta to punt-tipo
              move CsFc-cfa-vta to lFlgFcCs
              if tipo-cfa-vta = 0
                 beep
                 message  1, '****************'
                 message 17,    'Ya anulada esta '
                 if punt-bofa = 1
                    message 33, '    FACTURA     '
                 else
                    message 33, '    BOLETA      '
                 end-if
                 message 49, '****************'
                 wait 1 second
                 move ER to last-key
                 exit
              end-if
              perform Fc_Resta_Cargas
              DELETE RECORD FROM CFA-VTA-F.


             ;move 0 to  tipo-cfa-vta
             ;rewrite REG-CFA-VTA.
exit.


Fc_Confirma_Anula.
;-----------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '   Anulacion    '.
    message 49, '    Yes / No    '.
    action
             when ENTER move OK to last-key exit.
             when KEND  move ER to last-key exit.
             when NO    move ER to last-key exit.
    loop.


Fc_Resta_Cargas.
;---------------
perform Caj_Ini
if tipo-cfa-vta = 2
   subtract monto-adelanto from caja-arc-car  ;1.-
   subtract monto-adelanto from ade-arc-car   ;2.-
   move     monto-cfa-vta to  nSaldo          ;3.-
   subtract monto-adelanto from nSaldo
   subtract nSaldo from cre-arc-car
else
   subtract monto-cfa-vta from con-arc-car
   subtract monto-cfa-vta from caja-arc-car
end-if
rewrite REG-ARC-CAR
exit.


FC_ANULA_DET.
;-------------
   perform OPENUP-DET-VTA-F
   search FIRST for cFactura in DFA-VTA-F OFFSET IS nfac-dfa-vta
          when EOF exit.
          when ER exit.
   READ DFA-VTA-F
   move cod-pro-dfa-vta  to cCodPrd
   move can-pro-dfa-vta  to nCntPrd
   DELETE RECORD FROM DFA-VTA-F
   if lFlgFcCs = 1
      perform Iv_Sub_Prd_Rel
   else
      perform Fc-Aumenta-Inv
   end-if
   perform Fc_Sub_Ventas
   LOOP.


FC_BORRA_PENDIENTE.
;------------------
perform OPENUP-ARC-COB-F
search FIRST for cFactura in ARC-COB-F  OFFSET IS nr-doc-arc-cob
   when OK DELETE RECORD FROM ARC-COB-F.
exit.

Fc-Aumenta-Inv.
;--------------
perform Iv_Add_Producto
perform Iv_Sub_Prd_Rel
exit.


Iv_Sub_Prd_Rel.
;--------------
; Actualizar las relaciones de productos (CPG y CPC)

perform OPENREF-MAE-PRO-F
search FIRST for cCodPrd   in MAE-PRO-F
          when ER exit.
read MAE-PRO-F
move  relac1-mae-pro to nRelEnv1
move  relac2-mae-pro to nRelEnv2


if nRelEnv1 > 0 or nRelEnv2 > 0
   if nRelEnv1 > 0
      search FIRST for nRelEnv1 in MAE-PRO-F
             when OK read MAE-PRO-F
                     move nround-mae-pro  to  nNroUnd
                     perform Iv_Busca_Producto

                     move canti-arc-inv      to  nNumero1
                     move nCntPrd            to  nNumero2
                     perform Iv_Resta_Enteros
                     move nNumero1           to  canti-arc-inv
                     rewrite REG-ARC-INV.
   end-if

   if nRelEnv2 > 0
      search FIRST for nRelEnv2 in MAE-PRO-F
             when OK read MAE-PRO-F
                     move nround-mae-pro  to  nNroUnd
                     move nRelEnv2  to nRelEnv1
                     perform Iv_Busca_Producto

                     move canti-arc-inv      to  nNumero1
                     move nCntPrd            to  nNumero2
                     perform Iv_Resta_Enteros
                     move nNumero1           to  canti-arc-inv
                     rewrite REG-ARC-INV.
   end-if
   search FIRST for cCodPrd   in MAE-PRO-F
          when ER exit.
   read MAE-PRO-F
end-if
move OK to last-key
exit.




;FC-AUMENTA-INV.
;;---------------
;   perform OPENREF-ARC-INV
;        when ER exit.
;   seek FIRST in ARC-INV-F
;     when EOF exit.
;   search FIRST for cCodPrd in ARC-INV-F
;       when ER exit.
;   read ARC-INV-F
;
;   move canti-arc-inv      to     nNumero1       ; Disminuye Ventas.
;   move nCntPrd            to     nNumero2
;   perform Iv_Suma_Enteros
;   move nNumero1           to     canti-arc-inv
;   rewrite REG-ARC-INV
;
;   ; Actualizar las relaciones de productos (CPG y CPC)
;
;
;   perform OPENREF-MAE-PRO-F
;   search FIRST for cCodPrd   in MAE-PRO-F
;             when ER exit.
;   read MAE-PRO-F
;   move  relac1-mae-pro to nRelEnv1
;   move  relac2-mae-pro to nRelEnv2
;
;
;   if nRelEnv1 > 0 or nRelEnv2 > 0
;   ;  move 0 to lFlgIvCs
;      if nRelEnv1 > 0
;         search FIRST for nRelEnv1 in MAE-PRO-F
;                when OK read MAE-PRO-F
;                        move nround-mae-pro  to  nNroUnd
;                        perform Iv_Busca_Producto
;
;                        move canti-arc-inv      to  nNumero1
;                        move nCntPrd            to  nNumero2
;                        perform Iv_Resta_Enteros
;                        move nNumero1           to  canti-arc-inv
;                        rewrite REG-ARC-INV.
;      end-if
;
;      if nRelEnv2 > 0
;         search FIRST for nRelEnv2 in MAE-PRO-F
;                when OK read MAE-PRO-F
;                        move nround-mae-pro  to  nNroUnd
;                        move nRelEnv2  to nRelEnv1
;                        perform Iv_Busca_Producto
;
;                        move canti-arc-inv      to  nNumero1
;                        move nCntPrd            to  nNumero2
;                        perform Iv_Resta_Enteros
;                        move nNumero1           to  canti-arc-inv
;                        rewrite REG-ARC-INV.
;      end-if
;    ; if lFlgFcCs = 1
;    ;    move 1 to lFlgIvCs
;    ; end-if
;      search FIRST for cCodPrd   in MAE-PRO-F
;             when ER exit.
;      read MAE-PRO-F
;   end-if
;   move OK to last-key
;   exit.


Fc_Ingreso.
;-----------
   move 1  to punt-bofa
   perform FC-INICIALIZA


   move  0  to  lFlgPed 
   perform Fc_Ver_Ped                        ; Verificar Csg Pendiente.
           when Ok perform Fc_Cnf_Ped
                        when Ok  perform Fc_Recorre_Ped
                                 when Ok  move 1             to lFlgPed 
                                          move nro-doc-ped   to cNroDoc. ; Doc. Csg.
                                          
                        end-when
           end-when




   move  0  to  lFlgEvCs
   perform Fc_Ver_Csg                        ; Verificar Csg Pendiente.
           when Ok perform Fc_Cnf_Csg
                        when Ok  perform Fc_Recorre_Csg
                                 when Ok move env-det-csg to lFlgEvCs
                                         move nro-doc-det-csg to cNroDoc  ; Doc. Csg.
                                         perform Fc_Cargar_Csg.
                        end-when
           end-when

   perform BOLET-FACT
           when ER exit.

   ; para ver cual es el correlativo que se va a incrementar
  ;if flag-impresora = 'S'
  ;    move 16 to codigo-tab-fol          ; Ticket
  ;else
  ;    if punt-bofa = 1
  ;          move 1  to codigo-tab-fol    ; Facturas
  ;    else
  ;          move 10 to codigo-tab-fol    ; Boletas
  ;    end-if
  ;end-if


   if punt-bofa = 1
      move 1  to codigo-tab-fol    ; Facturas
   else
      move 10 to codigo-tab-fol    ; Boletas
   end-if

   perform Fc_Ver_Fact
         when ER exit.

   if cli-rut-ws = 0 and punt-bofa = 1
      perform FcVerRuc
              when ER exit.
   end-if

   if lFlgPed = 1
      perform Fc_Cargar_Ped.

   perform FcDetalle
   exit.


Fc_Ver_Fact.
;-----------
   PERFORM Lee-Folio
   perform ShowDocActual
         when ER exit.

   if punt-bofa = 1
      move 'F'            to cBoFa
      move 'F'            to cBoFa
   else
      move 'B'            to cBoFa
   end-if

   perform OPENUP-CFA-VTA-F
   search FIRST for cFactura in CFA-VTA-F OFFSET IS nfac-cfa-vta
      when OK  message 49, '   Ya Existe    '
               beep
               wait 1 second
               loop.



move OK to last-key
exit.


Fc_Cargar_Ped.
;-------------
perform OPENREF-PEDIDOS-F
search first for cNroDoc in  PEDIDOS-F OFFSET IS nro-doc-ped
      when Ok READ PEDIDOS-F.
next.

Fc_Cargar_Ped001.
;----------------
  if cNroDoc = nro-doc-ped    
     move  cod-pro-ped      to busca-pro-ws
     perform Busca-Cod-Prod
     move PRECIO-MAE-PRO to p-producto-ws           ; Precio producto.
     move PRECIO-MAE-PRO to nPrecioGral             ; Precio producto.
     move 0              to nDscPrd

     move  cod-pro-ped      to cCodPrd
     move  can-pre-ped      to nCntPrd

     move  cod-pro-ped   to nRelEnv1
     perform Iv_Busca_Producto

     if canti-arc-inv <  can-pre-ped
        if canti-arc-inv > 0
              move  canti-arc-inv to nCntPrd
              perform Fc_Cantidad_ped
        end-if
     else 
        perform Fc_Cantidad_ped
     end-if
  else
     exit
  end-if

SEEK NEXT IN  PEDIDOS-F
     when EOF exit.
     when ER  exit.
     when OK  READ PEDIDOS-F.
loop.

Fc_Cantidad_Ped.
;---------------
 perform FcTransItem
 if nDscPrd > 0
    move nAcumulado   to nAcumAuxC
    move nAcuIgv      to nAcuIgvAuxC

    move nPrecioGral  to p-producto-ws
    perform FcTransItem
    move nAcumulado   to nAcumDsc

    move nAcumAuxC    to nAcumulado
    move nAcuIgvAuxC  to nAcuIgv
 end-if
 perform FcAcumulaItem
 perform FcGrabaItem       ;Graba Item
      when ER exit.
exit.


Fc_Cargar_Csg.
;-------------
move   1   to    lFlgFcCs    ; /* Activa Consignaciขn */
move   1   to    lFlgIvCs    ; /* Activa Inventario para Consiganciขn */
perform OPENUP-ARC-INV-F
erase ARC-INV-F
perform OPENREF-DET-CSG-F
search first for cNroDoc in  DET-CSG-F OFFSET IS nro-doc-det-csg
      when Ok READ DET-CSG-F.
next.

Fc_Cargar_Csg001.
;----------------
  if cNroDoc = nro-doc-det-csg
     move  cod-pro-det-csg  to busca-pro-ws
     perform Busca-Cod-Prod
     move  cod-pro-det-csg  to cCodPrd
     move  can-pre-det-csg  to nCntPrd
     perform Iv_Add_Producto


  else
     exit
  end-if

SEEK NEXT IN  DET-CSG-F
     when EOF exit.
     when ER  exit.
     when OK  READ DET-CSG-F.
loop.

Fc_Ver_Csg.          ; Verifica si cliente tiene Consignaciขn Pendiente.
;----------
move 'CSGPEN'     to nombre-fn
move nCodVd       to codigo-fn
move ws-file-name to file-name
open REF DET-CSG-F
     when OK search FIRST for nCodCli  in DET-CSG-F
                    when EOF move ER to last-key.
                    when Ok  READ  DET-CSG-F
                             move cod-pro-det-csg to busca-pro-ws
                             perform Busca-Cod-Prod
                    end-when

     end-when
exit.

Fc_Ver_Ped.          ; Verifica si cliente tiene Consignaciขn Pendiente.
;----------
move 'PED-XX'     to nombre-fn
move nCodVd       to codigo-fn
move ws-file-name to file-name
open REF PEDIDOS-F
     when OK search FIRST for nCodCli  in PEDIDOS-F
                    when EOF move ER to last-key.
                    when Ok  READ  PEDIDOS-F
                             move cod-pro-ped     to busca-pro-ws
                             perform Busca-Cod-Prod
                    end-when

     end-when
exit.


Fc_Recorre_Csg.
;--------------
    move nro-doc-det-csg to cNroDoc
    move cod-pro-det-csg to cCodPrd
    move nround-mae-pro  to nNroUnd
    move descri-mae-pro  to d-producto-ws
    move can-pre-det-csg to can-producto-ws
    move fecha-det-csg   to fecha-emisi-ws

    move can-producto-ws to nStock
    move env-det-csg     to lFlgEvCs


    clear
    message 1, 'Consignacion:'
    if  lFlgEvCs = 1
        message 15, 'CE'
    else
        message 15, 'SE'
    end-if

    show CS-DOC-SHOW-SD
    perform lee-tecla-action
        when ENTER move Ok to last-key exit.
        when DAR   perform CSDV-AVANZA-PRODUCTO.
        when UAR   perform CSDV-ANTERIOR-PRODUCTO.
        when FAR   perform CSDV-MUESTRA-FEC-DOC.
        when KEND  move Er to last-key exit.
      loop.


Fc_Recorre_Ped.
;--------------
    move nro-doc-ped     to cNroDoc
    move cod-pro-ped     to cCodPrd
    move nround-mae-pro  to nNroUnd
    move descri-mae-pro  to d-producto-ws
    move can-pre-ped     to can-producto-ws
    move fecha-ped       to fecha-emisi-ws

    move can-producto-ws to nStock
   ;move env-ped         to lFlgEvCs


    clear
   ;message 1, 'Pedido      :'
   ;if  lFlgEvCs = 1
   ;    message 15, 'CE'
   ;else
   ;    message 15, 'SE'
   ;end-if

    show PE-DOC-SHOW-SD
    perform lee-tecla-action
        when ENTER move Ok to last-key exit.
        when DAR   perform PED-AVANZA-PRODUCTO.
        when UAR   perform PED-ANTERIOR-PRODUCTO.
        when FAR   perform CSDV-MUESTRA-FEC-DOC.
        when KEND  move Er to last-key exit.
      loop.



Fc_Cnf_Csg.
;----------
    message  1, '  Consignacion  '
    message 17, '   Pendiente    '
    message 33, '   Liquidar ?   '
    message 49, '    Yes / No    '
    action
             when ENTER move OK to last-key exit.
             when KEND  move ER to last-key exit.
             when NO    move ER to last-key exit.
    loop.

Fc_Cnf_Ped.
;----------
    message  1, '    Pedido      '
    message 17, '   Pendiente    '
    message 33, '   Facturar ?   '
    message 49, '    Yes / No    '
    action
             when ENTER move OK to last-key exit.
             when KEND  move ER to last-key exit.
             when NO    move ER to last-key exit.
    loop.


FcVerRuc.
;--------
   accept RUT-SD
          when KEND move ER to last-key exit.
          when ER   move ER to last-key exit.
   if cli-rut-ws = 0
      move ER to last-key exit.

   if cli-rut-ws > 0
      move OK to last-key exit.

loop.


FC-INICIALIZA.
;-------------
   PERFORM LeeHoraFecha                      ; Carga Variables con Fecha y Hora

   move ' '         to cSerieBanco
   move 0           to nBanco
   move 0           TO sw-graba-vta
   move 0           TO nAcumulado            ; Total Parcial
   move 0           TO nAcumuladoc           ;
   move 0           TO nDscPrd               ; Dscto de Producto.
   move 0           TO total-nota-ws         ; Total de factura
   move 0           to nPrcPrd               ; Precio Unitario.
   move 0           to nAcuIgv               ; Total del Igv.
   move 0           to var1-ws
   move 0           to var3-ws
   move 0           to var5-ws
   move 0           to nDescuento
   move 0           to nDias
   move 0           to punt-pago
   move 0           to nAcumDscPrd


   perform OPENREF-MAE-PRO-F
        when ER exit.
   seek first in MAE-PRO-F
        when EOF exit.
   read MAE-PRO-F
exit.


FcDetalle.
;---------
SEEK FIRST IN MAE-PRO-F
      when Ok READ MAE-PRO-F.


FcDetalle001.
;------------
    move CODIGO-MAE-PRO to cCodPrd                 ; Cขdigo Producto.
    move DESCRI-MAE-PRO to d-producto-ws           ; Descripciขn Producto.
    move nround-mae-pro to nNroUnd                 ;
    move PRECIO-MAE-PRO to p-producto-ws           ; Precio producto.
    move PRECIO-MAE-PRO to nPrecioGral             ; Precio producto.

    move 0              to sw-error
    move 0              to nDscPrd
    move 0              to can-producto-ws
    move 1              to punt-tipo

    PERFORM Muestra-Cant-Prod
            when ER exit.
    SHOW p-Productos-Sd
    perform Fc_Verifica_item
        when OK READ PRIN-02-F
                move p-02-can-prod to nCntPrd
                move p-02-dsc-prod to nDscPrd
                show CANT-PROD-SD
                move p-02-prc-prod to  p-producto-ws
                subtract nDscPrd   from p-producto-ws
                show FcCambiaPrecio-SD.
        when ER IF sw-error = 1 or can-producto-ws = 0
                   BEEP
                   MESSAGE 49, 'No hay en Stock '
                ELSE
                   MESSAGE 49, 'Stock : '
                   SHOW Solo-Hay-Sd
                END-IF
        end-when
    perform lee-tecla-fast-action
             when ENTER if sw-error = 1 loop.
                        perform Fc_Verifica_item
                            when OK perform FcBorraItemSpool.
                            when ER move OK to last-key.
                        perform ACEP-CAN-PROD
                             when ER LOOP.
                        perform FC_CONF_PROD
                             when ER loop.
                        perform FcTransItem
                        if nDscPrd > 0
                           move nAcumulado   to nAcumAuxC
                           move nAcuIgv      to nAcuIgvAuxC

                           move nPrecioGral  to p-producto-ws
                           perform FcTransItem
                           move nAcumulado   to nAcumDsc

                           move nAcumAuxC    to nAcumulado
                           move nAcuIgvAuxC  to nAcuIgv
                        end-if
                        perform FcAcumulaItem
                        perform FcGrabaItem       ;Graba Item
                             when ER exit.
                        perform AVANZA-PRODUCTO
                        loop.
             when KFIND perform ACEPTA-PRODUCTO.
             when DAR   perform AVANZA-PRODUCTO.
             when NO    perform AVANZA-PRODUCTO.
             when UAR   perform RETROC-PRODUCTO.
             when KEND  if total-nota-ws > 0
                           perform TOTALIZA-VENTA
                                when KEND loop.
                           perform RUTINA-PAGO          ; Contado o Credito
                                when ER loop.
                           perform CONF-NOTA-VENTA
                                 when ER perform Fc_Borra_Cab
                                         if punt-bofa = 1
                                            move 'F'            to cBoFa
                                         else
                                            move 'B'            to cBoFa
                                         end-if
                                         perform Fc_Elimina_Detalle
                                         exit
                                  end-when

                           perform Fc_Eventual
                           perform Fc_Graba_Resto
                           perform Ruti-Print-Fact
                                   when OK perform Fc_Graba
                                           if lFlgPed = 1
                                              perform  Fc_Borra_Ped
                                           end-if
                                           perform Fc-Actualiza-Folio
                                           perform Fc_Graba_Visita.
                                   when ER perform Fc_Borra_Cab
                                           if punt-bofa = 1
                                              move 'F'            to cBoFa
                                           else
                                              move 'B'            to cBoFa
                                           end-if
                                           perform Fc_Elimina_Detalle exit.

                        end-if
                        perform Fc_Borra_Cab
                        perform Fc_Borra_Det
                        exit.
             when KELIM perform Fc_Verifica_item
                            when OK perform FcBorraItemSpool.
             end-when
    loop.

Fc_Graba_Visita.
;---------------
 perform Caj_Ini
 add 1 to cli-con-car

 perform OPENREF-ARC-ATE-F
 seek LAST in ARC-ATE-F
 read ARC-ATE-F

 if estado-arc-ate = 0
    add     1    to cli-ate-car
    move    1    to estado-arc-ate
 end-if
 move       1    to venta-arc-ate
 rewrite REG-ARC-ATE
 rewrite REG-ARC-CAR
 exit.


Fc_Cnf_Liq_Csg.
;--------------
    message  1, '   LIQUIDAR     '
    message 17, '   toda  la     '
    message 33, '  Consignacion? '
    message 49, '    Yes / No    '
    action
             when ENTER move OK to last-key exit.
             when KEND  move ER to last-key exit.
             when NO    move ER to last-key exit.
    loop.

Fc_Elimina_Detalle.
;------------------
   perform OPENUP-PRIN-02-F
   search first for nNroDoc in PRIN-02-F offset is p-02-factura
          when EOF exit.
          when ER exit.
   READ PRIN-02-F
   move p-02-cod-prod to cCodPrd
   move p-02-can-prod to nCntPrd
   move p-02-uni-prod to nNroUnd
   DELETE RECORD FROM PRIN-02-F
   perform FC-AUMENTA-INV
  ;perform Fc_Sub_Ventas
   LOOP.



Fc_Eventual.
;------------
perform If_Eventual
        when ER exit.
move 1 to punt-ingreso
perform  Acepta_Eventuales
perform OPENUP-EVENTUALF
move nCodCli          to  codcli-eve-fc
move nNroDoc          to  nfac-eve-fc
if punt-bofa = 1
   move 'F'    to  bofa-eve-fc
else
   move 'B'    to  bofa-eve-fc
end-if
move cNomEve          to  nombre-eve-fc
move cDirEve          to  direcc-eve-fc
move x'0D'            to  cr-eve-fc.
move x'0A'            to  lf-eve-fc.
write REG-EVENTUALF

perform OPENREF-PRIN-01-F
search first for nNroDoc in PRIN-01-F offset is p-01-factura
READ PRIN-01-F
move cNomCli           to p-01-nomb-esta
move cDirCli           to p-01-direccion
rewrite REG-PRINT-01
exit.


Acepta_Eventuales.
;----------------------
    examine punt-ingreso
         when 0 move 1 to punt-ingreso loop.
         when 1 move 'N' to shift-char
                move ' ' to cNomEve
                accept NOMBRE-EVENTUAL-SD
           end-when
         when 2 move 'D' to shift-char
                move ' ' to cDirEve
                accept DIRE-EVENTUAL-SD
           end-when
         when 3 move '@' to shift-char  exit.
    no-op.
         when ENTER add 1 to punt-ingreso.
         when OK    add 1 to punt-ingreso.
         when FAR   subtract 1 from punt-ingreso.
         when bAR   add 1 to punt-ingreso.
         when KEND  move ER to last-key exit.
    loop.
exit.

If_Eventual.
;-----------
if nCodCliEv = 99
   move OK to last-key
else
   move ER to last-key
end-if
exit

FcBorraItemSpool.
;----------------
READ PRIN-02-F
move p-02-can-prod to nCntPrd
perform FcTransItem
perform FcEliminaItem
DELETE RECORD FROM PRIN-02-F
perform FC-AUMENTA-INV
;perform Fc_Sub_Ventas
exit.

FcTransItem.
;----------
   ;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
   ;ณ CanUnd   = (CanPrd - INT(CanPrd))*100/NroUnid                   ณ
   ;ณ Cantidad = INT(CanPrd) + CanUnd                                 ณ
   ;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู


move 0    to var1-ws
move 0    to var2-ws
move 0    to nAcumAux
move 0    to nAcuIgvAux

move   nCntPrd     to      nCantidad
perform Iv_Normal_Entero                      ; Devuelve en Resto1 el resto.
move   nCantidad   to      nCntPrd

if nResto1 > 0
   perform FcObtienePrecio
   move nAcuIgv     to nAcuIgvAux
   move nAcumulado  to nAcumAux
end-if

move   nCntPrd     to      nCanPrd
divide     100     into    nCanPrd
if nCanPrd = 0
    move p-producto-ws to      nPrecio         ; Obtener precio.
    move       1       to      nCanPrd
    perform FcSubtotal
    move nAcuIgvAux to nAcuIgv
    move nAcumAux   to nAcumulado
else
    move p-producto-ws to      nPrecio
   ;move   nCntPrd     to      nCanPrd
   ;divide     100     into    nCanPrd
    perform FcSubtotal
    if nResto1 > 0
       add  nAcuIgvAux to  nAcuIgv
       add  nAcumAux   to  nAcumulado
    end-if
end-if
exit.


Iv_Normal_Entero.
;-------------
move 0    to nResto
move 0    to nResto1

move   nCantidad   to      nEntero                      ; = 111
perform Iv_Entero                                       ; = 100
move nEntero       to      nCantidad                    ; = 100

if nResto > 0 and nNroUnd > 1
   move nResto      to      nEntero                     ; = 11
   divide nNroUnd   into    nEntero  remainder nResto1  ; = 0 resto 11
   multiply nEntero by      100                         ; =  000
   add nEntero      to      nCantidad                   ; =  000

   add nResto1      to      nCantidad
end-if
exit.

Iv_Entero.
;---------
divide 100         into    nEntero  remainder nResto    ; = 1  resto 11
multiply  nEntero  by      100                          ; = 100
exit.

Iv_Entero_Unidades.
;------------------
move nUnidades to nEntero
perform Iv_Entero
multiply nEntero by nNroUnd
multiply nResto by 100
add  nResto  to  nEntero
move nEntero to  nUnidades
exit.

Iv_Unidades_Entero.
;------------------
 move nUnidades     to    nEntero
 divide 100       into    nEntero
 divide nNroUnd   into    nEntero  remainder nResto
 multiply nEntero by      100
 add nResto       to      nEntero
 exit.

Iv_Suma_Enteros.
;-----------------
; nNumero1 ==> Primer  parametro
; nNumero2 ==> Segundo parametro
; nSuma    ==> Resultado

move   nNumero1     to     nCantidad
perform Iv_Normal_Entero
move   nCantidad   to      nNumero1

move   nNumero2    to      nCantidad
perform Iv_Normal_Entero
move   nCantidad   to      nNumero2

move   nNumero1     to      nUnidades
perform  Iv_Entero_Unidades
move   nUnidades   to      nNumero1

move   nNumero2    to      nUnidades
perform  Iv_Entero_Unidades
move   nUnidades   to      nNumero2

add   nNumero2     to      nNumero1
move  nNumero1     to      nUnidades
perform Iv_Unidades_Entero
move  nEntero      to      nNumero1
exit.


Iv_Resta_Enteros.
;-----------------
; nNumero1 ==> Primer  parametro
; nNumero2 ==> Segundo parametro
; nSuma    ==> Resultado

move   nNumero1     to     nCantidad
perform Iv_Normal_Entero
move   nCantidad   to      nNumero1

move   nNumero2    to      nCantidad
perform Iv_Normal_Entero
move   nCantidad   to      nNumero2

move   nNumero1     to      nUnidades
perform  Iv_Entero_Unidades
move   nUnidades   to      nNumero1

move   nNumero2    to      nUnidades
perform  Iv_Entero_Unidades
move   nUnidades   to      nNumero2

subtract nNumero2   from   nNumero1
move  nNumero1     to      nUnidades
perform Iv_Unidades_Entero
move  nEntero      to      nNumero1
exit.


FcObtienePrecio.
;---------------
 move p-producto-ws    to  nPrcPrd

 multiply nPrcPrd      by  100
 divide nNroUnd      into  nPrcPrd
 move nPrcPrd          to  nNumRed
 perform REDONDEA-CIFRA
 move nNumRed          to  nPrcPrd

 move nPrcPrd          to  nPrecio
 move nResto1          to  nCanPrd
 perform FcSubTotal

 divide 100          into  nAcuIgv
 divide 100          into  nAcumulado

exit.


;FcSubTotal.
;;----------
;   ;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;   ;ณ Total_Parcial = Precio * Cantidad             ณ
;   ;ณ Total_Acumula = Total_Parcial * 100/100 + IGV ณ
;   ;ณ Precio_Unitar = Total_Acumulado / Cantidad    ณ
;   ;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
;
;    move 18            to nIgv
;
;    move 0             to var1-ws
;    move 0             to var2-ws
;
;    move nPrecio       to var1-ws   ; Precio
;    move nCanPrd       to var2-ws   ; Cantidad
;
;    multiply var1-ws   by var2-ws   ; Precio * Cantidad * 100
;    multiply var1-ws   by 1000      ; Precio Bruto
;    move nIgv          to var2-ws   ;
;    add  100           to var2-ws   ; 100  +  Igv
;    divide var2-ws   into var1-ws   ; Acumulado = Total_Factura*100/(100+Igv)
;    move var1-ws       to nAcumulado
;
;
;    move nAcumulado    to var2-ws   ; Precio_uni = Acumulado / Cantidad
;    multiply   var2-ws by 100
;    move 0             to nPrcPrd
;    divide nCanPrd   into var2-ws
;    move var2-ws       to nPrcPrd
;
;    move nAcumulado    to var2-ws   ; I.G.V.
;    multiply var2-ws   by nIgv
;    divide 100       into var2-ws
;                                    ; Redondeo
;    move var2-ws       to nNumRed
;    perform REDONDEA-CIFRA
;    move nNumRed       to var2-ws
;
;    move nAcumulado    to nNumRed
;    perform REDONDEA-CIFRA
;    move nNumRed       to nAcumulado
;    move nAcumulado    to var1-ws
;
;
;    divide 10        into var2-ws
;    move var2-ws       to nAcuIgv
;
;    divide 10        into nAcumulado
;    exit.

FcSubTotal.
;----------
   ;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
   ;ณ Total_Parcial = Precio * Cantidad             ณ
   ;ณ Total_Acumula = Total_Parcial * 100/100 + IGV ณ
   ;ณ Precio_Unitar = Total_Acumulado / Cantidad    ณ
   ;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

    move 18            to nIgv

    move 0             to var1-ws
    move 0             to var2-ws

    move 0             to nAcumuladoT
    move nPrecio       to var1-ws   ; Precio
    move nCanPrd       to var2-ws   ; Cantidad

    multiply var1-ws   by var2-ws   ; Precio * Cantidad * 100
    move     var1-ws   to nAcumuladoT
    multiply var1-ws   by 1000      ; Precio Bruto
    move nIgv          to var2-ws   ;
    add  100           to var2-ws   ; 100  +  Igv
    divide var2-ws   into var1-ws   ; Acumulado = Total_Factura*100/(100+Igv)


    move var1-ws       to nNumRed
    perform REDONDEA-CIFRA
    move nNumRed       to var1-ws
    divide 10        into var1-ws
    move var1-ws       to nAcumulado



    subtract  nAcumulado  from nAcumuladoT   ; I.G.V.
    move nAcumuladoT   to var2-ws
    move var2-ws       to nAcuIgv

  ; move nAcumulado    to var2-ws   ; I.G.V.
  ; multiply var2-ws   by nIgv
  ; divide 100       into var2-ws


    move nAcumulado    to var2-ws   ; Precio_uni = Acumulado / Cantidad
    multiply   var2-ws by 1000
    move 0             to nPrcPrd
    divide nCanPrd   into var2-ws
    move var2-ws       to nPrcPrd


                                    ; Redondeo
   ;move var2-ws       to nNumRed
   ;perform REDONDEA-CIFRA
   ;move nNumRed       to var2-ws

   ;move nAcumulado    to nNumRed
   ;perform REDONDEA-CIFRA
   ;move nNumRed       to nAcumulado
    move nAcumulado    to var1-ws


   ;divide 10        into var2-ws
   ;move var2-ws       to nAcuIgv

   ;divide 10        into nAcumulado
    exit.

FcEliminaItem.
;-------------
  subtract nAcumulado  from total-nota-ws   ; Acumulado
  if nDscPrd > 0
     subtract nAcumDsc      from nAcumuladoc
     move nAcumuladoC       to nAcumDscPrd
     subtract total-nota-ws from  nAcumDscPrd
  else
     subtract nAcumulado    from nAcumuladoc
  end-if
  move total-nota-ws     to var1-ws
  subtract nAcuIgv     from var3-ws
  exit.

FcAcumulaItem.
;-------------
  add nAcumulado         to total-nota-ws   ; Acumulado
  if nDscPrd > 0
     add  nAcumDsc          to nAcumuladoc
     move nAcumuladoC       to nAcumDscPrd
     subtract total-nota-ws from  nAcumDscPrd    ; nAcumDscPrd = nDescuento
  else
     add  nAcumulado        to nAcumuladoc
  end-if
  move total-nota-ws     to var1-ws
  add nAcuIgv            to var3-ws
exit.

Fc_Calcula_Total.
;----------------
    move 0                 to var5-ws
    move total-nota-ws     to var5-ws
    add  var3-ws           to var5-ws
    exit.

TOTALIZA-VENTA.
;--------------
    perform Fc_Calcula_Total
;   clear
;   message  1, '   Descuento:   '
;   perform FcDescuento
;           when KEND exit.
    next.

TOTALIZA-V.
;----------
  ; if nDescuento > 0
  ;    subtract nDescuento from var5-ws.
    if nAcumDscPrd = 0
       show TOTAL-VENTA-SD
    else
       move nAcumDscPrd to nDescuento
       show TOTAL-VENTA1-SD
    end-if
  ; if nDescuento > 0
  ;    show FCDescuento-SD.
    perform lee-tecla-action
         when KMOD perform FcDescuento
                   perform Fc_Calcula_Total
                   loop.
    exit.



FcDescuento.
;-----------
accept FCDESCUENTO-SD
       when KEND exit.
if nDescuento >= var5-ws
   loop.
exit.


REDONDEA-CIFRA.
;--------------
    move nNumRed  to numero

    if numero-c > 4
       move 0     to numero-c
       if numero-b < 9
          add 1 to numero-b
       else
          move 0 to numero-b
          if numero-a < 9
             add 1 to numero-a
          else
             move 0 to numero-a
             add 1 to numero-1
          end-if
       end-if
    else
       move 0     to numero-c
    end-if
    move numero to nNumRed
    exit.

Fc_Verifica_Item.
;----------------
perform OPENUP-PRIN-02-F
move nNroDoc       to Documento
move cCodPrd       to Producto
search first for Clave_Envases in PRIN-02-F offset is p-02-factura
       when EOF move ER  to last-key.
exit.

Fc_Borra_Cab.
;------------
perform OPENUP-PRIN-01-F
search first for nNroDoc in PRIN-01-F offset is p-01-factura
       when OK DELETE RECORD FROM PRIN-01-F loop.

Fc_Borra_Cab1.
;------------
perform OPENUP-PRIN-16-F
search first for nNroDoc in PRIN-16-F offset is p-16-nfac
       when OK DELETE RECORD FROM PRIN-16-F loop.
exit.


Fc_Borra_Det.
;------------
perform OPENUP-PRIN-02-F
search first for nNroDoc in PRIN-02-F offset is p-02-factura
       when OK DELETE RECORD FROM PRIN-02-F loop.
exit.

Fc_Borra_Ped.
;------------
perform OPENUP-PEDIDOS-F
search first for cNroDoc in PEDIDOS-F offset is nro-doc-ped
       when OK DELETE RECORD FROM PEDIDOS-F loop.
exit.

FcFleteDscto.
;------------
MOVE 0 TO nFlete
ACCEPT FcFlete-SD
       WHEN  KEND MOVE ER TO LAST-KEY.
exit.



FcDscPrd.
;--------
ACCEPT FcDscPrd-SD
;if nDscPrd > 0
   subtract nDscPrd from p-producto-ws
;end-if
SHOW FcCambiaPrecio-SD
exit.


FC_CONF_PROD.
;------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '    Producto    '.
    message 49, '    Yes / No    '.
    action
             when ENTER move OK to last-key exit.
             when KEND  move ER to last-key exit.
             when NO    move ER to last-key exit.
    loop.


Muestra-Cant-Prod.
;-----------------
    perform OPENREF-ARC-INV   ; Abrir Inventario.
         when ER move 0 to can-producto-ws exit.
    seek FIRST in ARC-INV-F
         when EOF move ER to last-key exit.
    search FIRST for cCodPrd in ARC-INV-F
        when ER move OK to last-key
                move 0 to can-producto-ws
                move 1 to sw-error
                exit.
    read ARC-INV-F
    move canti-arc-inv to can-producto-ws
    move OK to last-key
    exit.

AVANZA-PRODUCTO.
;---------------
    seek NEXT in MAE-PRO-F
         when EOF message 49, '-Fin Productos -'
                  beep
                 ;wait 1 second
                  seek last in MAE-PRO-F.
         when OK  read MAE-PRO-F.
    exit.

RETROC-PRODUCTO.
;---------------
    seek PRIOR in MAE-PRO-F
         when EOF message 49, '-Fin Productos -'
                  beep
                 ;wait 1 second
                  seek first in MAE-PRO-F.
         when OK  read MAE-PRO-F.
    exit.

ACEP-CAN-PROD.
;-------------
    move 0 to nCntPrd.
    accept CAN-PRODUCTO-SD.
        when OK  if nCntPrd = 0  MOVE ER TO LAST-KEY exit.
                  move 0 to sw-error
                  move ER to last-key
                  perform CHEQUEA-CANTIDAD
                          when ER loop.
                  if sw-error = 1 MOVE ER TO LAST-KEY.
                  exit.
        when KEND MOVE ER TO LAST-KEY exit.
        when KMOD  perform FcDscPrd.
    loop.

FcGrabaItem.
;-----------
    if sw-graba-vta = 0
       perform MUEVE-DATOS-VTA     ; Mueve datos a spool.
    end-if

    perform OPENUP-PRIN-02-F
    move nCodCli           to p-02-codigo
    move nNroDoc           to p-02-factura
    if punt-bofa = 1
       move 'F'            to p-02-bofa
    else
       move 'B'            to p-02-bofa
    end-if
    move cCodPrd           to p-02-cod-prod
    move nPrcPrd           to p-02-pre-prod
    move nCntPrd           to p-02-can-prod
    move nNroUnd           to p-02-uni-prod
    move d-producto-ws     to p-02-des-prod
    if nDscPrd     = 0
       move nAcumulado        to p-02-par-prod
    else
       move nAcumDsc          to p-02-par-prod
    end-if
    move p-producto-ws     to p-02-prc-prod
    move nDscPrd           to p-02-dsc-prod

    write REG-PRINT-02
    perform FC-DESCUENTA-INV
;   perform Fc_Add_Ventas
    exit.

CHEQUEA-CANTIDAD.
;----------------
    perform OPENREF-ARC-INV
         when ER exit.
    seek FIRST in ARC-INV-F
         when EOF move ER to last-key exit.
    search FIRST for cCodPrd in ARC-INV-F
         when ER beep
                 message 49, 'No hay en Stock '
                 wait 1 seconds
                 move 1 to sw-error
                 exit.
    read ARC-INV-F
    move canti-arc-inv to nStock
    if nStock = 0
       beep
       message 49, ' No queda Stock '
       wait 1 seconds
       move ER to last-key
       move 1 to sw-error
       exit
    end-if

    move nCntPrd to nCantidad
    perform Iv_Normal_Entero
    move nCantidad to nCntprd

    if nCntPrd > canti-arc-inv
       beep
       message 49, 'Solo Hay : '
       show SOLO-HAY-SD
       wait 1 seconds
       move ER to last-key
       move 1 to sw-error
       exit
    end-if
    move OK to last-key
    exit.

MUEVE-DATOS-VTA.                   ; Datos a Cabecera-spool.
;---------------

    PERFORM LeeHoraFecha
    perform OPENUP-PRIN-01-F
    move dia-normal-ws     to p-01-dia.
    move mes-normal-ws     to p-01-mes.
    move ano-normal-ws     to p-01-ano.
    move hrs-normal-ws     to p-01-hora.
    move min-normal-ws     to p-01-minu.
    move seg-normal-ws     to p-01-segu.
    move nCodCli           to p-01-codigo.
    if punt-bofa = 1
       move 'F'            to p-01-bofa
    else
       move 'B'            to p-01-bofa
    end-if
    move cli-rut-ws        to p-01-ruc
    move cNomcli           to p-01-nomb-esta
    move cDircli           to p-01-direccion
    move nCodVd            to p-01-vendedor
    move nNroDoc           to p-01-factura
    move 0                 to p-01-monto-1
    move 0                 to p-01-monto-2
    move 0                 to p-01-monto-3
    move 0                 to p-01-ind-adelanto
    move 0                 to p-01-monto-adelan
    move ' '               to p-01-cheque
    move 0                 to p-01-banco
    write REG-PRINT-01.
    move 1 to sw-graba-vta.
    exit.


FC-ACTUALIZA-FOLIO.
;-------------------
    ;if punt-bofa =  1
    ;   move '01' to codigo-tab-fol
    ;else
    ;   move '10' to codigo-tab-fol
    ;end-if


    ; para ver si se actualzia la factura, la boleta o el ticket
    if flag-impresora = 'S'
        move '16' to codigo-tab-fol
    else
        if punt-bofa = 1
             move '01' to codigo-tab-fol
        else
             move '10' to codigo-tab-fol
        end-if
    end-if

    search FIRST for codigo-tab-fol in TAB-FOL-F
    read TAB-FOL-F
    add 1  to folio-tab-fol
    rewrite REG-TAB-FOL
    exit.


CONF-NOTA-VENTA.
;---------------
    message  1, '*   Confirma   *'
    message 17, '*   Venta al   *'
    if punt-tipo = 1
       message 33, '*   Contado    *'
    else
       message 33, '*   Credito    *'
    end-if
    message 49, '*   Yes / No   *'
    beep
    perform lee-tecla-action
        when ENTER exit.
        when NO    move 1 to swich-venta
                   MOVE ER TO LAST-KEY
                   exit.
    loop.



GRABA-TIPO-FACTURA.
;------------------
    move punt-tipo to ws-01-tipo.
    perform OPENREF-PRIN-01-F
         when ER exit.
    seek first in PRIN-01-F.
      when EOF exit.
    search first for nNroDoc        in PRIN-01-F offset is p-01-factura
        when ER exit.
    read PRIN-01-F.
    move punt-tipo to p-01-tipo.
    rewrite REG-PRINT-01
    exit.

Fc_Graba.
;--------
    if lFlgFcCs = 1                             ; Para los envases de
       move nNroDoc  to Documento
       move '15' to codigo-tab-fol
       perform LEE-FOLIO
       add   1  to folio-tab-fol
       rewrite  REG-TAB-FOL
       move  folio-tab-fol  to cDocEnv
       move Documento to nNroDoc
    end-if
    perform Fc_Graba_Cabecera
    perform Fc_Graba_Detalle
    if lFlgFcCs = 1                             ; Para los envases de
       perform FcDscRst
    end-if
    move var5-ws to saldo-ws
    exit.

Fc_Graba_Resto.
;--------------
    perform OPENUP-PRIN-01-F
    search first for nNroDoc        in PRIN-01-F offset is p-01-factura
        when ER exit.
    read PRIN-01-F
    move var1-ws           to p-01-monto-1
    move var3-ws           to p-01-monto-2
    move var5-ws           to p-01-monto-3
    move nDescuento        to p-01-monto-dsc
    move nDias             to p-01-dias
    rewrite REG-PRINT-01
exit.


Fc_Graba_Cabecera.
;-----------------
    perform OPENUP-PRIN-01-F
    search first for nNroDoc        in PRIN-01-F offset is p-01-factura
        when ER exit.
    read PRIN-01-F

    perform OPENUP-CFA-VTA-F
    move p-01-codigo        to codigo-cfa-vta
    move p-01-factura       to nfac-cfa-vta
    move p-01-bofa          to bofa-cfa-vta
    move p-01-dia           to dia-cfa-vta
    move p-01-mes           to mes-cfa-vta
    move p-01-ano           to ano-cfa-vta
    move p-01-hora          to hora-cfa-vta
    move p-01-minu          to minu-cfa-vta
    move p-01-segu          to segu-cfa-vta
    move p-01-vendedor      to c-ven-cfa-vta
    move p-01-monto-3       to monto-cfa-vta
    move p-01-tipo          to tipo-cfa-vta
    move p-01-monto-2       to igv-cfa-vta
    move p-01-monto-dsc     to dsc-cfa-vta
    move nRuta              to ruta-cfa-vta
    move p-01-ind-adelanto  to ind-adelanto
    move p-01-monto-adelan  to monto-adelanto
    move p-01-cheque        to cheque-cfa-vta
    move p-01-banco         to bco-cfa-vta
    move p-01-dias          to dias-cfa-vta
    move lFlgFcCs           to CsFc-cfa-vta
    move x'0D'              to cr-cfa-vta
    move x'0A'              to lf-cfa-vta
    write REG-CFA-VTA.

    perform OPENUP-PRIN-16-F
    search first for nNroDoc        in PRIN-16-F offset is p-16-nfac
        when OK read PRIN-16-F
                perform OPENUP-PAGOSF
                move p-16-codigo       to  codigo-pag
                move p-16-nfac         to  nfac-pag
                move p-16-bofa         to  bofa-pag
                move p-16-monto-che    to  monto-che-pag
                move p-16-monto-efe    to  monto-efe-pag
                move p-16-bco          to  bco-pag
                move p-16-nro-chq      to  nro-chq-pag
                move p-16-date         to  date-pag
                move p-16-hora         to  hora-pag
                move x'0D'             to  cr-pag
                move x'0A'             to  lf-pag
                write REG-PAGOSF.

    if p-01-tipo = 2
        perform OPENUP-ARC-COB-F
        move p-01-codigo     to   codigo-arc-cob           ; Cขdigo del Cliente
        move p-01-factura    to   doc-ref-cob              ; Nฃmero de Factura
        move p-01-bofa       to   bofa-cob                 ; Boleta o Factura.
        move date-cfa-vta    to   fec-e-arc-cob            ; Fecha de Emisiขn
        perform Fc_Cal_Fec_Venc
        move fecha-venci-ws  to   fec-v-arc-cob            ; Fecha de Vencimiento
        move p-01-monto-3    to   nSaldo
        subtract p-01-monto-adelan from nSaldo
        move nSaldo          to   m-ade-arc-cob
        write  REG-ARC-COB
    end-if
       perform Fc_Act_Caj

    exit.

Fc_Cal_Fec_Venc.
;---------------
perform LeeHoraFecha
move nFecha   to fecha-venci-ws

move 31 to nDiasMes(01)
move 28 to nDiasMes(02)
move 31 to nDiasMes(03)
move 30 to nDiasMes(04)
move 31 to nDiasMes(05)
move 30 to nDiasMes(06)
move 31 to nDiasMes(07)
move 31 to nDiasMes(08)
move 30 to nDiasMes(09)
move 31 to nDiasMes(10)
move 30 to nDiasMes(11)
move 31 to nDiasMes(12)


;dia-venci-ws
;mes-venci-ws
;ano-venci-ws
;nDias  ==>   Dias de Crdito

add    ndias           to      dia-venci-ws
move   ano-venci-ws    to      nEntero
divide 4               into    nEntero  remainder nResto    ; = 0 resto 11
if nResto = 0
   move 29 to nDiasMes(02)
end-if
move  mes-venci-ws  to nDiaIndice
if dia-venci-ws  > nDiasMes(nDiaIndice)
   perform  Fc_Cal_Fec
end-if
exit.


Fc_Cal_Fec.
;---------
move  mes-venci-ws  to nDiaIndice
if dia-venci-ws  > nDiasMes(nDiaIndice)
   subtract nDiasMes(nDiaIndice) from dia-venci-ws
   add 1    to mes-venci-ws
   if mes-venci-ws > 12
      subtract 12 from mes-venci-ws
      add 1    to  ano-venci-ws
      move   ano-venci-ws    to      nEntero
      divide 4               into    nEntero  remainder nResto    ; = 0 resto 11
      if nResto = 0
         move 29 to nDiasMes(02)
      else
         move 28 to nDiasMes(02)
      end-if
   end-if
   loop
else
   exit.

Fc_Act_Caj.
;----------
perform Caj_Ini
if p-01-tipo = 2                           ; Credito
   add p-01-monto-adelan to ade-arc-car
   add p-01-monto-adelan to caja-arc-car
   add nSaldo            to cre-arc-car
else                                       ; Contado
   add p-01-monto-3 to con-arc-car
   add p-01-monto-3 to caja-arc-car
end-if
if punt-pago = 3
   add nMontoEfe         to caja-arc-car
end-if
rewrite REG-ARC-CAR
exit.



Fc_Borra_Cabecera.
;-----------------
perform OPENUP-PRIN-01-F
search first for nNroDoc in PRIN-01-F  offset is p-01-factura
       when OK DELETE RECORD FROM PRIN-01-F.
exit.


Fc_Graba_Detalle.
;----------------
    perform OPENUP-PRIN-02-F
    search first for nNroDoc        in PRIN-02-F offset is p-02-factura
        when ER exit.

Fc_Graba_Det001.
;---------------
    read PRIN-02-F
    if p-02-factura = nNroDoc
       perform OPENUP-DET-VTA-F
       move p-02-codigo     to codigo-dfa-vta
       move p-02-factura    to nfac-dfa-vta
       move p-02-bofa       to bofa-dfa-vta
       move p-02-cod-prod   to cod-pro-dfa-vta
       move p-02-can-prod   to can-pro-dfa-vta
       move p-02-pre-prod   to pre-uni-dfa-vta
       move p-02-par-prod   to acu-pro-dfa-vta
       move nFecha          to date-dfa-vta
       move x'0D'           to cr-dfa-vta
       move x'0A'           to lf-dfa-vta
       write REG-DFA-VTA

       move 0               to lFlgIvCs

       move p-02-cod-prod   to nRelEnv1
       perform Iv_Busca_Producto

       move p-02-cod-prod   to cCodPrd
       move p-02-uni-prod   to nNroUnd
       move p-02-can-prod   to nCntPrd
       perform Fc_Cs_Envases                       ; Busca si tiene envases
                                                   ; Pendientes
       move p-02-cod-prod   to cCodPrd
       move p-02-uni-prod   to nNroUnd
       move p-02-can-prod   to nCntPrd
       perform Fc_Add_Ventas
       if lFlgFcCs = 1                             ; Para los envases de
          perform FcDscCsg                         ; Consignaciขn.
       end-if

       seek NEXT in PRIN-02-F
            when EOF exit.
       loop
    end-if
    exit.


Fc_Cs_Envases.
;-------------
  ; Actualizar las relaciones de productos (CPG y CPC)

  move cCodPrd to Producto
  perform OPENREF-MAE-PRO-F
  search FIRST for cCodPrd   in MAE-PRO-F
            when ER exit.
  read MAE-PRO-F
  move  relac1-mae-pro to nRelEnv1
  move  relac2-mae-pro to nRelEnv2

  if nRelEnv1 > 0 or nRelEnv2 > 0
     if nRelEnv1 > 0
        search FIRST for nRelEnv1 in MAE-PRO-F
               when OK read MAE-PRO-F
                       move p-02-can-prod   to  nCntPrd
                       move nround-mae-pro  to  nNroUnd
                       move nRelEnv1        to  cCodPrd
                       perform  Verifica_Env_Pend.
        move Producto to cCodPrd
     end-if

     if nRelEnv2 > 0
        search FIRST for nRelEnv2 in MAE-PRO-F
               when OK read MAE-PRO-F
                       move p-02-can-prod   to nCntPrd
                       move nround-mae-pro  to nNroUnd
                       move nRelEnv2        to cCodPrd
                       perform  Verifica_Env_Pend.
        move Producto to cCodPrd
     end-if
     search FIRST for cCodPrd   in MAE-PRO-F
            when ER exit.
     read MAE-PRO-F
  end-if
  move OK to last-key
  exit.


Verifica_Env_Pend.
;-----------------
perform Verifica_Env-Csg
       when ER exit.
       when OK  read ENV-CSG-F
                if nCntPrd > can-pre-env-csg
                   DELETE RECORD FROM ENV-CSG-F
                   move can-pre-env-csg   to nCntPrd
                   perform Iv_Sub_Producto
                else
                   move can-pre-env-csg   to nStock

                   move can-pre-env-csg   to nNumero1
                   move nCntPrd           to nNumero2
                   perform Iv_Resta_Enteros
                   move nNumero1          to can-pre-env-csg
                   if nNumero1 = 0
                      DELETE RECORD FROM ENV-CSG-F
                   else
                      rewrite  REG-ENV-CSG
                   end-if

                   perform Iv_Sub_Producto
                   move nStock    to   nCntPrd
                end-if
       end-when
exit.



Verifica_Env-Csg.
;----------------
perform OPENUP-ENV-CSG-F
move nCodCli       to Cliente
move cCodPrd       to ProductoCsg
search first for Clave_consignacion in ENV-CSG-F  ; offset is nro-doc-env-csg
       when EOF move ER  to last-key.
exit.



FcDscRst.
;-----------
move 0 to lFlgIvCs
perform Fc_Graba_Cs_Prd      ; Graba Devoluciขn.
perform OPENUP-DET-CSG-F     ; Consignaciones Pendientes
search first for cNroDoc in DET-CSG-F OFFSET IS nro-doc-det-csg
    when ER exit.
    when EOF exit.
    when Ok  READ DET-CSG-F
             DELETE RECORD FROM DET-CSG-F
             move can-pre-det-csg to nCntPrd    ; Aคade resto a inventa.
             move cod-pro-det-csg to cCodPrd    ;
             move cCodPrd         to busca-pro-ws
             perform Busca-Cod-Prod
             perform Iv_Add_FcDevCs
             perform Iv_Add_Producto
             if lFlgEvCs = 1                      ; Con Envases
                perform Iv_Add_Prd_Rel
             else
                move  cod-pro-det-csg  to p-15-cod-prod
                move  can-pre-det-csg  to p-15-cnt-prod
                move  nNroUnd          to p-15-und-prod

                move p-15-cod-prod to  cCodPrd
                move p-15-cnt-prod to  nCntPrd
                move p-15-und-prod to  nNroUnd
                perform Cs_Add_Prd_Rel
             end-if
             loop
    end-when
exit.


FcDscCsg.
;-------
move 0 to lFlgIvCs
perform Fc_Graba_Cs_Prd              ; Graba Devoluciขn.
if lFlgEvCs = 1
   perform Iv_Add_Prd_Rel
end-if

perform OPENUP-DET-CSG-F     ; Consignaciones Pendientes
move cNroDoc to  Documento
move cCodPrd to  Producto
search first for Clave_envases in DET-CSG-F OFFSET IS nro-doc-det-csg
    when ER exit.
    when EOF exit.
    when Ok  READ DET-CSG-F
             perform Iv_Add_FcDevCs
             move can-pre-det-csg to nNumero1     ; Devolver el resto.
             move nCntPrd         to nNumero2
             perform Iv_Resta_Enteros
             move nNumero1        to can-pre-det-csg
             DELETE RECORD FROM DET-CSG-F

             if nCntPrd  > 0
                move cCodPrd        to busca-pro-ws
                perform Busca-Cod-Prod
                move nNumero1        to nCntPrd           ; Aคade resto a inventa.
                perform Iv_Add_Producto
                if lFlgEvCs = 0                  ; Sin Envases.
                   move  cod-pro-det-csg  to p-15-cod-prod
                   move  can-pre-det-csg  to p-15-cnt-prod
                   move  nNroUnd          to p-15-und-prod

                   move p-15-cod-prod to  cCodPrd
                   move p-15-cnt-prod to  nCntPrd
                   move p-15-und-prod to  nNroUnd
                   perform Cs_Add_Prd_Rel
                end-if
             end-if

    end-when
exit.

Iv_Add_FcDevCs.
;-----------------
perform OPENREF-ARC-INV
search FIRST for cCodPrd in ARC-INV-F
       when OK  read ARC-INV-F
                move csr-arc-inv     to nNumero1
                move can-pre-det-csg to nNumero2
                perform Iv_Suma_Enteros
                move nNumero1        to csr-arc-inv
                rewrite REG-ARC-INV.
exit.


Fc_Graba_Cs_Prd.
;---------------
perform OPENUP-DEV-CSG-F
move cNroDoc         to nro-doc-csg           ;
move cDocEnv         to doc-ref-csg           ; De Prstamo.
move nCodCli         to cod-cli-csg
move cCodPrd         to cod-pro-csg
move nCntPrd         to can-pre-csg
move nFecha          to fecha-csg
move nCodVd          to vend-csg
move nRuta           to ruta-csg
move x'0D'           TO cr-csg
move x'0A'           TO lf-csg
write REG-DEV-CSG
exit.


Fc_Add_Ventas.
;-------------
perform OPENREF-ARC-INV
search FIRST for cCodPrd in ARC-INV-F
       when OK  read ARC-INV-F
                if punt-tipo = 1
                   move vco-arc-inv     to nNumero1               ;Aumenta Cajas Vendidas Contado
                   move nCntPrd         to nNumero2
                   perform Iv_Suma_Enteros
                   move nNumero1        to vco-arc-inv
                   rewrite REG-ARC-INV
                else
                   move vcr-arc-inv     to nNumero1               ;Aumenta Cajas Vendidas Crdito
                   move nCntPrd         to nNumero2
                   perform Iv_Suma_Enteros
                   move nNumero1        to vcr-arc-inv
                   rewrite REG-ARC-INV
                end-if
       end-when
exit.

Fc_Sub_Ventas.
;-------------
perform OPENREF-ARC-INV
search FIRST for cCodPrd in ARC-INV-F
       when OK  read ARC-INV-F
                if punt-tipo = 1
                   move vco-arc-inv     to nNumero1               ;Aumenta Cajas Vendidas Contado
                   move nCntPrd         to nNumero2
                   perform Iv_Resta_Enteros
                   move nNumero1        to vco-arc-inv
                   rewrite REG-ARC-INV
                else
                   move vcr-arc-inv     to nNumero1               ;Aumenta Cajas Vendidas Crdito
                   move nCntPrd         to nNumero2
                   perform Iv_Resta_Enteros
                   move nNumero1        to vcr-arc-inv
                   rewrite REG-ARC-INV
                end-if
       end-when
exit.


Fc_Borra_Detalle.
;-----------------
perform OPENUP-PRIN-02-F
search first for nNroDoc in PRIN-02-F  offset is p-02-factura
       when OK DELETE RECORD FROM PRIN-02-F.
exit.

Fc-Descuenta-Inv.
;----------------
perform Iv_Sub_Producto
perform Iv_Add_Prd_Rel
exit.


Iv_Add_Prd_Rel.
;--------------
  ; Actualizar las relaciones de productos (CPG y CPC)

  perform OPENREF-MAE-PRO-F
  search FIRST for cCodPrd   in MAE-PRO-F
            when ER exit.
  read MAE-PRO-F
  move  relac1-mae-pro to nRelEnv1
  move  relac2-mae-pro to nRelEnv2

  if nRelEnv1 > 0 or nRelEnv2 > 0
     if nRelEnv1 > 0
        search FIRST for nRelEnv1 in MAE-PRO-F
               when OK read MAE-PRO-F
                       move nround-mae-pro  to  nNroUnd
                       perform Iv_Busca_Producto

                       move canti-arc-inv      to  nNumero1
                       move nCntPrd            to  nNumero2
                       perform Iv_Suma_Enteros
                       move nNumero1           to  canti-arc-inv
                       rewrite REG-ARC-INV.
     end-if

     if nRelEnv2 > 0
        search FIRST for nRelEnv2 in MAE-PRO-F
               when OK read MAE-PRO-F
                       move nround-mae-pro  to  nNroUnd
                       move nRelEnv2  to nRelEnv1
                       perform Iv_Busca_Producto

                       move canti-arc-inv      to  nNumero1
                       move nCntPrd            to  nNumero2
                       perform Iv_Suma_Enteros
                       move nNumero1           to  canti-arc-inv
                       rewrite REG-ARC-INV.
     end-if
     search FIRST for cCodPrd   in MAE-PRO-F
            when ER exit.
     read MAE-PRO-F
  end-if
  move OK to last-key
  exit.


Iv_Busca_Producto.
;-----------------
perform OPENUP-ARC-INV-F
search FIRST for nRelEnv1 in ARC-INV-F
       when ER  move nRelEnv1     to  codigo-arc-inv
                move 0            to  canti-arc-inv
                move 0            to  ini-arc-inv
                move 0            to  vco-arc-inv
                move 0            to  vcr-arc-inv
                move 0            to  pen-arc-inv
                move 0            to  csr-arc-inv
                move 0            to  cso-arc-inv
                move 0            to  den-arc-inv
                move 0            to  obs-arc-inv
                move 0            to  cjs-arc-inv
                move 0            to  rts-arc-inv
                move 0            to  dsc-arc-inv
                move 0            to  recarga-inv
                move 0            to  den-arc-inv
;               move nRuta        to  rut-arc-inv
                move nFecha       to  fec-arc-inv
                move nCodVd       to  ven-arc-inv
                move x'0D'        to  cr-inv
                move x'0A'        to  lf-inv
                write REG-ARC-INV
                loop.
       when OK  read ARC-INV-F.
exit.


Iv_Add_Producto.
;-----------------
perform OPENUP-ARC-INV-F
search FIRST for cCodPrd in ARC-INV-F
       when ER  move cCodPrd      to  codigo-arc-inv
                move nCntPrd      to  canti-arc-inv
               ;move nCntPrd      to  ini-arc-inv
                move 0            to  ini-arc-inv
                move 0            to  vco-arc-inv
                move 0            to  vcr-arc-inv
                move 0            to  pen-arc-inv
                move 0            to  cso-arc-inv
                move 0            to  csr-arc-inv
                move 0            to  den-arc-inv
                move 0            to  obs-arc-inv
                move 0            to  cjs-arc-inv
                move 0            to  rts-arc-inv
                move 0            to  dsc-arc-inv
                move 0            to  recarga-inv
      ;         move nRuta        to  rut-arc-inv
                move nFecha       to  fec-arc-inv
                move nCodVd       to  ven-arc-inv
                move x'0D'        to  cr-inv
                move x'0A'        to  lf-inv
                write REG-ARC-INV.
       when OK  read ARC-INV-F
                move canti-arc-inv   to nNumero1
                move nCntPrd         to nNumero2
                perform Iv_Suma_Enteros
                move nNumero1        to canti-arc-inv
                rewrite REG-ARC-INV.

exit.


Iv_Sub_Producto.
;-----------------
perform OPENUP-ARC-INV-F
search FIRST for cCodPrd in ARC-INV-F
       when OK  read ARC-INV-F
                move canti-arc-inv   to nNumero1
                move nCntPrd         to nNumero2
                perform Iv_Resta_Enteros
                move nNumero1        to canti-arc-inv
                rewrite REG-ARC-INV.
exit.



ANULA-NOTA.
;----------
    reset PRIN-01-F
    reset PRIN-02-F
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                 RUTINA PARA ELEGIR BOLETAS Y FACTURAS.                     ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
BOLET-FACT.
;-----------
    message 1,  '****************'.
    message 49, '****************'.
    examine punt-bofa
            when 0 move 2 to punt-bofa                     loop.
            when 1 message 17 '     Factura                    '.
            when 2 message 17 '     Boleta                     '.
            when 3 move 1 to punt-bofa                     loop.
    message 33, '    Yes / No    '.
    fast-action
            when ENTER move OK to last-key exit.
            when KEND  move ER to last-key exit.
            when NO    add 1 to punt-bofa.
            when FAR   add 1 to punt-bofa.
            when BAR   subtract 1 from punt-bofa.
    loop.



;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                 RUTINA PARA ACEPTAR LOS PAGOS DE FACTURAS                  ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
RUTINA-PAGO.
;-----------
    move 1 to punt-pago
    message 1,  '****************'.
    examine punt-tipo
            when 0 move 2 to punt-tipo      loop.
            when 1 message 17 '     Contado                    '.
            when 2 message 17 '     Credito                    '.
            when 3 move 1 to punt-tipo      loop.
    message 49, '    Yes / No    '.
    fast-action
            when ENTER perform GRABA-TIPO-FACTURA
                       examine punt-tipo
                               when 1 perform FORMA_PAGO      ; Contado
                                              when ER loop.
                               end-when
                               when 2 perform CANCELA-CREDITO ; Crdito
                                              when ER loop.
                                      if pagoa-cuenta-ws <> 0
                                         perform FORMA_PAGO
                                              when ER loop.
                                      end-if
                                      PERFORM FcGrabaAdelanto
                               end-when
                        exit.
            when KEND  MOVE ER TO LAST-KEY EXIT.
            when NO    add 1 to punt-tipo.
            when FAR   add 1 to punt-tipo.
            when BAR   subtract 1 from punt-tipo.
    loop.

FcGrabaAdelanto.
;----------------
;move ind-adelanto  to p-01-ind-adelanto
move punt-pago      to p-01-ind-adelanto
move monto-adelanto to p-01-monto-adelan
rewrite REG-PRINT-01
exit.


FORMA_PAGO.
;----------
    message 1,  '****************'.
    examine punt-pago
            when 0 move 3 to punt-pago      loop.
            when 1 message 17 '    Efectivo                    '.
            when 2 message 17 '     Cheque                     '.
            when 3 message 17 '    Cheque y        Efectivo    '.
            when 4 move 1 to punt-pago      loop.
    message 49, '    Yes / No    '.
    fast-action
            when ENTER perform PAGO-001
                       exit.
            when KEND  MOVE ER TO LAST-KEY exit.
            when NO    add 1 to punt-pago.
            when FAR   add 1 to punt-pago.
            when BAR   subtract 1 from punt-pago.
    loop.


PAGO-001.
;--------
   examine punt-pago
        when 1 perform CONTADO-EFECTIVO.  ; Efectivo
        when 2 perform GRABA-PAGO-UNO.    ; Cheque
        when 3 perform GRABA-PAGO-UNO.    ; Cheque y Efectivo.
    move OK to last-key
    exit.


CONTADO-EFECTIVO.
;----------------
    if punt-tipo = 1                      ; Contado ==> Cancelaciขn.
       move var5-ws to cancela-ws
       show CANCELA-SD
   else                                   ; Credito ==> Adelanto.
       move var5-ws to cancela-ws
       subtract monto-adeuda-ws from cancela-ws
       show CANCELA-SD
       move monto-adeuda-ws to nSaldo
       show FcMuestraSaldo-Sd
    end-if
    perform lee-tecla-action
        when ENTER subtract cancela-ws from saldo-ws
                   perform GRABA-PAGO-UNO
                   exit.
        when KEND  exit.
    loop.

CANCELA-CREDITO.
;---------------
    move 0 to monto-adeuda-ws.
    move var5-ws to monto-adeuda-ws.
    move 1 to punt-ingreso.
    perform ACCEP-DATOS-CREDITO.       ;siempre invoca a pago-a-cuenta
         when ER exit.
    PERFORM LeeHoraFecha
    move pagoa-cuenta-ws   to ws-01-monto.
    exit.

ACCEP-DATOS-CREDITO.
;-------------------
    examine punt-ingreso
         when 0 move 1 to punt-ingreso loop.
         when 1 move 8 to nDias
                show DIAS-VENCI-SD
                perform lee-tecla-action
                    when FAR accept DIAS-VENCI-SD
                                 when OK   move OK to last-key.
                                 when KEND MOVE ER TO LAST-KEY EXIT.
                      end-when
                    when ENTER move OK to last-key.
                    when KEND  MOVE ER TO LAST-KEY EXIT.
           end-when
         when 2 move 0 to pagoa-cuenta-ws
                move 0 to monto-adelanto
                accept PAGOA-CUENTA-SD
                    when KEND MOVE ER TO LAST-KEY EXIT.
                    when OK if pagoa-cuenta-ws >= monto-adeuda-ws loop.
                            subtract pagoa-cuenta-ws   from  saldo-ws
                            move pagoa-cuenta-ws       to    monto-adelanto
                            subtract pagoa-cuenta-ws   from  monto-adeuda-ws
                            subtract pagoa-cuenta-ws   from  cancela-ws
                            move OK to last-key.
           end-when
         when 3 show MONTO-ADEUDA-SD
                perform lee-tecla-action
                    when ENTER move OK to last-key.
           end-when
         when 4 exit.
    no-op.
         when OK  add 1 to punt-ingreso.
         when FAR subtract 1 from punt-ingreso.
    loop.

GRABA-PAGO-UNO.
;--------------
    examine punt-pago
         when 2 perform GRABA-CHEQUE.    ; Cheque
         when 3 perform GRABA-CHEQUE     ; Cheque + Efectivo
                     when ER exit.
         end-when
    exit.

GRABA-CHEQUE.
;------------
    move ' ' to cSerieBanco.
    accept SERIE-BCO-SD
        when OK if cSerieBanco = ' ' loop.
                perform INGRESA-BANCO
                     when ER exit.
                perform Fc_Act_Chq.
                perform Fc_Graba_Pagos
                exit
          end-when
    loop.

Fc_Act_Chq.
;----------
 perform OPENREF-PRIN-01-F   ; Actualiza en Spool
      when ER exit.          ; Bco y Cheque
 search first for nNroDoc in PRIN-01-F offset is p-01-factura
     when ER exit.
 read PRIN-01-F
 move cSerieBanco    to p-01-cheque
 move nBanco         to p-01-banco
 rewrite REG-PRINT-01
exit.

Fc_Graba_Pagos.
;--------------
; Var5-ws ==> Contiene el total de la Factura

perform OPENUP-PRIN-16-F
move nCodCli       to  p-16-codigo
move nNroDoc       to  p-16-nfac
if punt-bofa = 1
   move 'F'            to cBoFa
else
   move 'B'            to cBoFa
end-if
move cBofa         to  p-16-bofa
move nMontoChq     to  p-16-monto-che
move nMontoEfe     to  p-16-monto-efe
move nBanco        to  p-16-bco
move cSerieBanco   to  p-16-nro-chq
move nFecha        to  p-16-date
move nHora         to  p-16-hora
write REG-PRINT-16
exit.

INGRESA-BANCO.
;-------------
    move 0 to nBanco
   ;accept CODIGO-BANCO-SD
    perform Acepta-Banco
        when ER exit.
        when OK if nBanco          = 0 loop.
                if punt-pago = 3
                   accept MONTO-CHEQ-SD
                   if nMontoChq >= var5-ws
                      loop
                   else
                      move var5-ws         to nMontoEfe
                      subtract nMontoChq from nMontoEfe
                   end-if
                end-if
                if punt-pago = 2
                   if punt-tipo = 1          ;Contado
                      move var5-ws to nMontoChq
                   else
                      move pagoa-cuenta-ws nMontoChq
                   end-if

                   show MONTO-CHEQ-SD
                   perform lee-tecla-action
                end-if
                exit.
    loop.


ACEPTA-BANCO.
;------------
    perform OPENREF-BANCOS
            when ER exit.
    seek FIRST in BANCOS-F
            when EOF message 1,  '****************'
                     message 17, ' Archivo Bancos '
                     message 33, '     Vacio      '
                     message 49, '****************'
                     wait 1 second
                     exit.
    read BANCOS-F.

ACEPTA-BANCO-001.
;------------------
   seek FIRST in BANCOS-F
        when OK read BANCOS-F.
   perform  Acepta-CodBanco.
            when OK exit.
            when ER exit.
exit.


Acepta-CodBanco.
;------------------
    accept CODIGO-BANCO-SD
         when KEND   move ER to last-key    exit.
         when OK     search FIRST for nBanco  in BANCOS-F
                         when OK read BANCOS-F
                                 perform Acepta_Banco
                                         when OK exit.

              end-when
         when UAR   seek LAST in BANCOS-F
                    read BANCOS-F
                    perform Acepta_Banco
                            when OK exit.
                    exit.
         when DAR   seek FIRST in BANCOS-F
                    read BANCOS-F
                    perform REC-BANCOS
                            when OK exit.
                    exit.
    loop.

Acepta_Banco.
;------------
    move bc-codigo to nBanco.
    show CODIGO-BANCO-SD
    show DESC-BANCO-SD.
    fast-action
         when KEND   move ER to last-key    exit.
         when ENTER move OK to last-key exit.
         when UAR   seek PRIOR in BANCOS-F
                        when EOF beep
                                 message 49, ' Inicio Bancos  '
                                 wait 1 second
                                 seek FIRST in BANCOS-F.
                    read BANCOS-F.
         when DAR   seek NEXT in BANCOS-F
                        when EOF beep
                                 message 49, '  Fin Bancos    '
                                 wait 1 second
                                 seek LAST in BANCOS-F.
                    read BANCOS-F.
    loop.



MOTIVO-NO-VENTA.
;---------------
    perform OPENREF-TAB-MOT-F
         when ER exit.
    seek FIRST in TAB-MOT-F
         when EOF beep
                  message  1, '****************'
                  message 17, 'Archivo  Motivos'
                  message 33, '     Vacio      '
                  message 49, '****************'
                  wait 1 second
                  exit.
    read TAB-MOT-F.

MOTIVO-NO-001.
;-------------
    message  1, '   Motivo No    '.
    message 17, '    Reparto     '.
    message 49, '    Yes / No    '.
    show MOTIVO-SD
    perform lee-tecla-action
       when ENTER perform GRABA-NO-VENTA exit.
       when NO    seek NEXT in TAB-MOT-F
                    when EOF seek FIRST in TAB-MOT-F.
                  read TAB-MOT-F.
       when FAR   seek NEXT in TAB-MOT-F
                    when EOF seek FIRST in TAB-MOT-F.
                  read TAB-MOT-F.
       when BAR   seek PRIOR in TAB-MOT-F
                    when EOF seek LAST in TAB-MOT-F.
                  read TAB-MOT-F.
       when KEXIT perform Borra_No_Venta exit.
    loop.

Borra_No_Venta.
;-------------
perform OPENUP-ARC-ATE-F
seek LAST in ARC-ATE-F
     when Ok DELETE RECORD FROM ARC-ATE-F.
exit.


GRABA-NO-VENTA.
;--------------
    perform OPENREF-ARC-ATE-F.
    seek LAST in ARC-ATE-F
    read ARC-ATE-F
    perform LeeHoraFecha
    move nHora             to HoraF-arc-ate
    move ncomp-atendido    to estado-arc-ate
    move ncomp-codigo      to motivo-arc-ate
    rewrite REG-ARC-ATE
    if nComp-atendido = 1
       perform Caj_Ini
       add 1 to cli-ate-car
       rewrite REG-ARC-CAR
    end-if
    exit.




;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ             RUTINA DE IMPRESION DE FACTURA              ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
RUTI-PRINT-FACT.
;---------------
    perform SETEA-IMPRESORA
            when ER exit.
            when KEND move OK to last-key exit.
    clear
    message 1, '* IMPRIMIENDO *'
    perform OPENREF-PRIN-01-F
         when ER exit.
    next.

IMP-FACT-001.
;------------

    perform OPENREF-PRIN-01-F
    search first for nNroDoc in PRIN-01-F offset is p-01-factura
                     when EOF move ER  to last-key.
    READ PRIN-01-F
    build CAB-FACT-000   print
    build CABECERA-RUC   print

    if flag-impresora = 'S'
       move " TICKET NUMERO : " to cComprobante
    else
       if p-01-bofa = "F"
          move "       FACTURA : " to cComprobante
       else
          move "  BOLETA VENTA : " to cComprobante.
       end-if
    end-if

    ;movemos el codigo de la factura a la estructura w-01-factura para impr.
    move p-01-factura to w-01-factura

    build CAB-FACT-001   print                  ; Imprime CABECERAS
    build CAB-FACT-002   print
    build CAB-FACT-003   print
    if p-01-bofa = "F"
       build CAB-FACT-006   print.  ; Ruc Clientes
    build CAB-FACT-007   print
    if lFlgFcCs = 1
       build CAB-FACT-008   print
    end-if

    build RAYA   print
    build CAB-FACT-005   print
    if p-01-tipo    = 1
       move 'Contado' to var6-ws
    else
       move 'Credito' to var6-ws.
    perform OPENREF-PRIN-02-F.
         when ER exit.
    search first for nNroDoc in PRIN-02-F offset is p-02-factura
                     when EOF move ER  to last-key.

    perform IMP-FACT-002   ; Detalle de la Factura.
         when ER exit.

   ;if nAcumuladoc = 0
    if p-01-monto-dsc = 0
       move p-01-monto-1 to CAMPO14
       move p-01-monto-2 to CAMPO15
       move p-01-monto-3 to CAMPO16
       if p-01-bofa = "F"
          build DET-FACT-002
       end-if
    else
       move nAcumuladoC  to CAMPO17
       move nAcumDscPrd  to CAMPO18

       move p-01-monto-1 to CAMPO14
       move p-01-monto-2 to CAMPO15
       move p-01-monto-3 to CAMPO16
       if p-01-bofa = "F"
          build DET-FACT-002-1
       end-if
    end-if
   ;if p-01-monto-dsc > 0  and  p-01-bofa = "F"
   ;   build DET-FACT-006.

    if p-01-bofa = "B"
       build Cr_Lf
       build Cr_Lf.

    build DET-FACT-005
    build DET-FACT-003
    if p-01-monto-adelan > 0
       build det-fact-003-1 print.
    seek next in PRIN-02-F
      when eof perform IMP-FACT-003 exit.
    loop.

IMP-FACT-002.
    read PRIN-02-F
    if p-02-factura = p-01-factura
       if p-01-bofa = "F"
          build DET-FACT-001
       else
          build DET-FACT-001-1
       end-if
       print
    end-if
    seek next in PRIN-02-F.
      when eof exit.
    loop.

IMP-FACT-003.
    build FIRMA-VEND-CLIENTE print.
    build FIN-LISTADO.   print.
    disconnect.                                 ; Termino Comunicaciones IP-40
    exit.

;ีอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                      RUTINA DE COBRANZA EN LA RUTA                        ณ
;ิอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
COBRANZAS.
;---------
    PERFORM CbMueveFolio
   ;PERFORM ShowDocActual
   ;        when ER exit.
    move 0     to nBanco
    move ' '   to cSerieBanco
    move ' '              to cNomEve
    move ' '              to cDirEve
    move n-estb-mae-cli   to cNomCli
    move direcc-mae-cli   to cDirCli
    PERFORM CbBuscaCobranza     ; Si cliente tiene pendiente.
      when ER  beep
               message  1, '****************'
               message 17, 'Cliente NO tiene'
               message 33, '    Cobranza    '
               message 49, '****************'
               wait 1 seconds
               exit.
    PERFORM CbMuestraCobranza   ; Rutina Central
    exit.

CbBuscaCobranza.
;---------------
    perform OPENREF-ARC-COB-F
         when ER exit.
         when EOF move ER to last-key exit.
    seek first in ARC-COB-F
         when EOF move ER to last-key exit.
    search FIRST for nCodCli           in ARC-COB-F
      when OK  read ARC-COB-F.
exit.


CbMueveFolio.
;--------------
 move  04  to codigo-tab-fol
 perform LEE-FOLIO
 exit.


CbMuestraCobranza.
;----------------

    move codigo-arc-cob        to      nCodCli                ; Cliente
    move nr-doc-arc-cob        to      ws-01-fact             ; Factura
    move fec-e-arc-cob         to      fecha-emisi-ws         ; Fecha
    move fec-v-arc-cob         to      fecha-venci-ws         ; Fecha Vencimiento

    move m-ade-arc-cob         to      ws-01-monto            ; Monto Adeudado
    move ws-01-monto           to      nSaldo
    move ws-01-monto           to      monto-adeuda-ws

    show COBRANZA-SD
    if ws-01-monto <> 0
       show COBRANZA-SALDO-SD
    else
       message 49, '   Cancelada    '.

    fast-action.
        when ENTER clear
                   if m-ade-arc-cob = 0  loop.
                   move 1 to punt-cobra
                   perform CbTipMov            ; 2=Amortizacion,1=Cancelacion.
                        when ER exit.
                        when KEND loop.
                   if punt-cobra = 2            ; Amortizaciขn
                      perform AMORTIZA-COBRANZA
                           when ER loop.
                   else
                      move 0 to nSaldo
                   end-if
                   clear
                   move 1 to punt-cancela
                   PERFORM CbEfectivoCheque     ; Efectivo -cheque
                        when ER exit.
                        when KEND loop.
                   PERFORM CbConfirma
                        WHEN ER perform Cb_Borra_Todo EXIT.
                   perform IMPRIME-COBRANZA
                           when OK perform Cb_Graba
                                   perform ACTUALIZA-REG-COBRANZA
                                   perform CB-ACTUALIZA-FOLIO.
                           WHEN ER perform Cb_Borra_Todo.
                   exit.
        when UAR   perform RETROC-COBRANZA.
        when DAR   perform AVANZA-COBRANZA.
        when KEND  exit.
    loop.

Cb_Graba.
;--------
perform LeeHoraFecha
perform OPENUP-PRIN-12-F
search first for nNroDoc in PRIN-12-F
       when ER exit.
       when OK read PRIN-12-F.

perform OPENUP-DCO-PAG-F
MOVE p-12-documento    TO nfac-dco-pag
MOVE p-12-cod-cli      TO codigo-dco-pag
MOVE p-12-tipo-pago    TO tipo-dco-pag
MOVE p-12-monto        TO monto-dco-pag
MOVE p-12-factura      TO ndoc-dco-pag
MOVE p-12-tipo-dinero  TO tipo-dco-din
MOVE p-12-cheque       TO cheque-dco-pag
MOVE p-12-bco          TO bco-dco-pag
MOVE nFecha            TO fecha-dco-pag
MOVE nCodVd            TO vend-dco-pag
MOVE nRuta             TO ruta-dco-pag
move x'0D'             TO cr-dco-pag
move x'0A'             TO lf-dco-pag
write REG-DCO-PAG
perform Cb_Act_Caj
perform Cb_Borra
move ' ' to cSerieBanco.
move 0 to nBanco         .
loop.

Cb_Act_Caj.
;---------
perform Caj_Ini
add p-12-monto to cob-arc-car
add p-12-monto to caja-arc-car
rewrite REG-ARC-CAR
exit.


Cb_Borra.
;--------
perform OPENUP-PRIN-12-F
search first for nNroDoc in PRIN-12-F
       when OK DELETE RECORD FROM PRIN-12-F.
exit.

Cb_Borra_todo.
;-------------
perform Cb_Borra
       when OK loop.
exit.

Cb_Cheque.
;---------
move ' ' to cSerieBanco.
move 0 to nBanco         .
accept SERIE-BCO-SD
    when OK if cSerieBanco = ' ' loop.
            move ws-01-monto to nMontoChq
            perform Cb_Banco
                 when ER exit.
            exit
      end-when
    when KEND exit.
loop.

Cb_Banco.
;--------
   ;accept CODIGO-BANCO-SD
    perform Acepta-Banco
        when OK if nBanco          = 0 loop.
                show MONTO-CHEQ-SD
                perform lee-tecla-action
                exit.
    loop.


CbConfirma.
;-------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '    Cobranza    '.
    message 49, '    Yes / No    '.
    action
             when ENTER move 1 to nAtencion
                        move OK to last-key exit.
             when KEND  move ER to last-key exit.
             when NO    move ER to last-key exit.
    loop.


AVANZA-COBRANZA.
;---------------
    search NEXT for nCodCli           in ARC-COB-F
         when ER  message 49, '- No Tiene Mas -'
                  beep
                  wait 1 second.
         when OK  read ARC-COB-F.
    exit.

RETROC-COBRANZA.
;---------------
    perform seek-prior-cobranza until nCodCli = codigo-arc-cob or last-key = eof
    if last-key = eof
       message 49 '- Primera Cobr.-'
       beep
       wait 1 second
       search first for nCodCli           ARC-COB-F
       when ok read ARC-COB-F.
    end-if
    exit.

seek-prior-cobranza.
;-----------------
   seek prior in ARC-COB-F
   when ok read ARC-COB-F.
   exit.

CbTipMov.
;--------
    message 1,  '****************'.
    examine punt-cobra
            when 0 move 2 to punt-cobra     loop.
            when 1 message 17 '  Cancelacion                   '.
            when 2 message 17 '  Amortizacion                  '.
            when 3 move 1 to punt-cobra     loop.
    message 49, '    Yes / No    '.
    perform lee-tecla-fast-action
            when ENTER exit.
            when KEND  exit.
            when NO    add 1 to punt-cobra.
            when FAR   add 1 to punt-cobra.
            when BAR   subtract 1 from punt-cobra.
    loop.

AMORTIZA-COBRANZA.
;-----------------
    move 0 to amortiza-ws.
    accept AMORTIZA-SD
        when OK    if amortiza-ws  = 0 loop.
                   if amortiza-ws >= ws-01-monto loop.
                   move amortiza-ws to ws-01-monto
                   subtract amortiza-ws from nSaldo
                   exit.
        when KEND  move ER to last-key, exit.
    loop.

CbEfectivoCheque.
;----------------
    message 1,  '****************'.
    examine punt-cancela
            when 0 move 2 to punt-cancela   loop.
            when 1 message 17 '    Efectivo                    '.
            when 2 message 17 '     Cheque                     '.
            when 3 move 1 to punt-cancela   loop.
    message 49, '    Yes / No    '.
    perform lee-tecla-fast-action
            when ENTER  if punt-cancela = 1           ; Efectivo
                           PERFORM Efectivo-Cobranza  ; Muestra Cantidad
                                   when ER loop.
                                   when ENTER perform GRABA-COBRANZA.
                        else
                           perform Cb_Cheque
                                   when KEND loop.
                           PERFORM GRABA-COBRANZA
                                   WHEN ER LOOP.
                        end-if
                        EXIT.
            when KEND  exit.
            when NO    add 1 to punt-cancela.
            when FAR   add 1 to punt-cancela.
            when BAR   subtract 1 from punt-cancela.
    loop.


EFECTIVO-COBRANZA.
;-----------------
    move ws-01-monto to cancela-ws
    show CANCELA-SD
    show CbMuestraSaldo-Sd
    action
        when ENTER move punt-cancela to punt-pago
                   exit.
        when KEND  exit.
    loop.


GRABA-COBRANZA.
;---------------
 perform OPENUP-PRIN-12-F
 move nNroDoc          to p-12-documento       ; Documento
 move nCodCli          to p-12-cod-cli         ; Cliente
 move nr-doc-arc-cob   to p-12-factura         ; Factura Inicial
 move punt-cobra       to p-12-tipo-pago       ; 1=Cancela 2=Amortiza
 move punt-cancela     to p-12-tipo-dinero     ; 1=Efectivo 2=Cheque
 move ws-01-monto      to p-12-monto           ; Monto
 move punt-cancela     to tipo-dco-pag
 move cSerieBanco     to p-12-cheque
 move nBanco           to p-12-bco

 move cNomcli          to p-12-nom-cli
 move cDirCli          to p-12-dir-cli
 move dia-ws           to p-12-dia
 move mes-ws           to p-12-mes
 move ano-ws           to p-12-ano
 move hrs-ws           to p-12-hora
 move minu-ws          to p-12-minu
 move sec-ws           to p-12-segu
 write  REG-PRINT-12

 if punt-cancela = 2
    perform OPENUP-ARC-CHE-F
    move nCodCli          to codigo-arc-che
    move nNroDoc          to nrodoc-arc-che
                             ; BoFa-arc-che
    move cSerieBanco     to serie-arc-che
    move nBanco           to c-bco-arc-che
    move nFecha           to fecha-arc-che
    move ws-01-monto      to monto-arc-che
    write REG-ARC-CHE
 end-if
 exit.


ACTUALIZA-REG-COBRANZA.
;---------------
    perform OPENUP-ARC-COB-F.
         when ER exit.
    seek first in ARC-COB-F.
      when EOF exit.
    search FIRST for ws-01-fact    in ARC-COB-F OFFSET nr-doc-arc-cob
      when ER exit.
      when OK  read ARC-COB-F
               move     m-ade-arc-cob to ws-02-monto
               subtract ws-01-monto from ws-02-monto
               if ws-02-monto = 0
                  DELETE RECORD FROM ARC-COB-F
               else
                  move     ws-02-monto to  m-ade-arc-cob
                  rewrite  REG-ARC-COB
               end-if
       end-when
    exit.


CB-ACTUALIZA-FOLIO.
;------------------
    move '04' to codigo-tab-fol            ; COBRANZAS
    search FIRST for codigo-tab-fol in TAB-FOL-F
    add 1  to folio-tab-fol
    rewrite REG-TAB-FOL
    exit.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ               REPORTE DE COBRANZA                       ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

IMPRIME-COBRANZA.
;----------------
    perform SETEA-IMPRESORA
            when ER exit.
            when KEND move OK to last-key exit.
    clear
    message 1, '* IMPRIMIENDO *'.
    next.

IMP-RECIBO-COBRANZA.
;-----------------------
    perform OPENUP-PRIN-12-F
    search first for nNroDoc in PRIN-12-F
    READ PRIN-12-F
    build CAB-FACT-000       print
    build CAB-COBR-001       print
    build CAB-COBR-002       print
    if nBanco > 0
    build CAB-COBR-003       print.

    build FIRMA-VEND-CLIENTE print
    build FIN-LISTADO        print
    disconnect                                  ; Termino Comunicaciones IP-40
    exit.


;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                 RECUPERACION DE ENVASES                 ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
ENVASES.
;-------
    move 1 to punt-pres.

ENVAS-001.
;---------
    move 'Prest. Envases ' to proc-ws.
    message 1,  '****************'.
    examine punt-pres
            when 0 move 2 to punt-pres      loop.
            when 1 message 17 '   Devolucion                  '.
            when 2 message 17 '    Prestamo                   '.
            when 3 move 1 to punt-pres      loop.
    message 33, '    Yes / No    '.
    perform lee-tecla-fast-action
            when ENTER perform ENVAS-002
                       exit.
            when KEND  exit.
            when NO    add 1 to punt-pres.
            when FAR   add 1 to punt-pres.
            when BAR   subtract 1 from punt-pres.
    loop.

ENVAS-002.
;---------
    examine punt-pres
            when 1 perform DEVOLUCIONES.
            when 2 move 1 to lFlgEvCs
                   perform PRESTAMOS.
    exit.


;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                     Devolucion de Envases                              ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

DEVOLUCIONES.
;------------
    PERFORM LeeHoraFecha                       ; Lee Hora y Fecha en Variables.

    perform OPENREF-DET-ENV-F.
         when ER exit.
    seek first in DET-ENV-F.
      when EOF exit.
    search FIRST for nCodCli           in DET-ENV-F
      when ER  beep
               message  1, '****************'
               message 17, 'Cliente NO  Debe'
               message 33, '    Envases     '
               message 49, '****************'
               wait 1 seconds.

      when OK  read DET-ENV-F
               move cod-pro-det-env to busca-pro-ws
               perform BUSCA-COD-PROD
                    when ER exit.
               perform DV-MUEVE-FOLIO
              ;PERFORM ShowDocActual
              ;     when ER exit.
               perform Dv_Muestra
                    when ER exit.
      end-when

    exit.

DV-MUEVE-FOLIO.
;--------------
    move '03' to codigo-tab-fol
    perform LEE-FOLIO
    exit.

Dv_Muestra.
;----------
    move nro-doc-det-env to cNroDoc
    move cod-pro-det-env to cCodPrd
    move nround-mae-pro  to nNroUnd
    move descri-mae-pro  to d-producto-ws
    move can-pre-det-env to can-producto-ws
    move fecha-det-env   to fecha-emisi-ws

    move can-producto-ws to nStock

    show ENVASES-DOS-SD
    perform Dv_Verifica_item
            when OK READ PRIN-09-F
                    move p-09-cnt-prod     to nCntPrd
                    move nStock            to nNumero1
                    move p-09-cnt-prod     to nNumero2
                    perform Iv_Resta_Enteros
                    move nNumero1          to nStock
                    show ENVASES-TRES-SD
                    show CANT-PRODUCTO-SD.
            when ER show ENVASES-TRES-SD
                    perform DV-MUESTRA-DOCUMENTO.

    perform lee-tecla-action
        when ENTER if can-producto-ws > 0
                      perform Dv_Borra_Item
                      perform Dv_Acepta_Cantidad
                              when ER loop.
                      perform Dv_Confirma_Item      ; Confirma Item ?
                              when ER loop.
                      perform Dv_Graba_Spool
                      perform DV-AVANZA-PRODUCTO
                   else
                      beep
                   end-if
        end-when
        when DAR   perform DV-AVANZA-PRODUCTO.
        when UAR   perform DV-ANTERIOR-PRODUCTO.
        when FAR   perform DV-MUESTRA-FEC-DOC.
        when KEND  perform Dv_Ver_Doc
                       when ER exit.
                   perform DV-CONFI-DV
                       when ER perform Dv_Borra_Todo exit.
                   perform DV-IMPRIMIR
                           when OK perform Dv_Graba
                                   perform DV-ACTUALIZA-FOLIO.
                           when ER perform Dv_Borra_Todo.
                   exit.
        when KELIM perform Dv_Borra_Item.

      loop.

Dv_Borra_Item.
;-------------
perform Dv_Verifica_item
   when OK DELETE RECORD FROM PRIN-09-F.
exit.

Dv_Ver_Doc.
;----------
perform OPENUP-PRIN-09-F
search FIRST for nNroDoc in PRIN-09-F OFFSET IS p-09-documento
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.


Dv_Verifica_Item.
;----------------
perform OPENUP-PRIN-09-F
move cNroDoc       to Documento
move cCodPrd       to Producto
search first for Clave_Envases in PRIN-09-F
       when EOF move ER  to last-key.
exit.


DV-BUSCA-DEVOLUCION.
;-------------------
search FIRST for nNroDoc in DEV-ENV-F
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.


DV-MUESTRA-DOCUMENTO.
;--------------------
if can-producto-ws <= 0
   message 49, '  Ya devueltos  '
   beep
   wait 1 second
end-if
show DV-SHOW-DOC
exit.

DV-MUESTRA-FEC-DOC.
;------------------
show ENVASES-UNO-SD
action
     when enter move OK to last-key
                exit.
     when KEND exit.
loop.

SEEK-PRIOR-DET-ENV.
;-----------------
   seek prior in DET-ENV-F
   when ok read DET-ENV-F.
EXIT.


dv-primer-producto.
;-----------------
   search first for nCodCli           in DET-ENV-F
   when er move eof to last-key.
   when ok read DET-ENV-F
           move cod-pro-det-env to busca-pro-ws
           perform BUSCA-COD-PROD
                   when ER exit.
   end-when
exit.

DV-AVANZA-PRODUCTO.
;------------------
search next for nCodCli           in DET-ENV-F
when er message 49, '-Fin de archivo-'
        beep
        wait 1 second
        perform Dv_ultimo_producto.

when ok read DET-ENV-F
        move cod-pro-det-env to busca-pro-ws
        perform BUSCA-COD-PROD
                when ER exit.
        end-when
exit.

Dv_ultimo_producto.
;------------------
search first for Clave_Envases in DET-ENV-F
       when EOF move ER  to last-key.
exit.



DV-ANTERIOR-PRODUCTO.
;---------------------
   perform seek-prior-det-env until nCodCli = codigo-det-env or last-key = eof
   if last-key = ok
      move cod-pro-det-env to busca-pro-ws
      perform BUSCA-COD-PROD
              when ER exit.
   else
      message 49, '-Ini de archivo-'
      beep
      wait 1 second
      perform dv-primer-producto
      move eof to last-key
   end-if
exit.

Dv_Acepta_Cantidad.
;------------------
    move  can-producto-ws to nCntPrd
    if nCntPrd <= 0
       exit.
    move 0  to nCntPrd
    accept CANT-PRODUCTO-SD
        when OK    if nCntPrd = 0 loop.
                   move nCntPrd to nCantidad
                   perform Iv_Normal_Entero
                   move nCantidad to nCntPrd

                   if nCntPrd > can-producto-ws
                      beep
                      message 49, ' Cantidad Mayor '
                      wait 1 seconds
                      move ER to last-key
                   end-if
                   exit
        end-when
        when KEND  move ER TO last-key  exit.
    loop.


DV-CONFI-DV.
;-------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '    Devolucion  '.
    message 49, '    Yes / No    '.
    action
             when ENTER move 1 to nAtencion
                        move OK to last-key exit.

             when KEND  move ER to last-key exit.
             when NO    move ER to last-key exit.
    loop.

DV-ACTUALIZA-FOLIO.
;--------------
    move '03' to ws-cod-folio              ; 03 para DEVOLUCIONES.
    search FIRST for ws-cod-folio in TAB-FOL-F
    add 1  to folio-tab-fol
    rewrite REG-TAB-FOL
    exit.

Dv_Confirma_Item.
;----------------
    message  1, '****************'.
    message 17, 'Confirma Ingreso'.
    message 33, '    Yes / No    '.
    message 49, '****************'.
    perform lee-tecla-action
        when ENTER exit.
        when NO    move ER to last-key exit.
    loop.


DV_Graba_Spool.
;--------------
perform OPENUP-PRIN-09-F

move  nNroDoc           to   p-09-documento      ; Documento de la Devoluciขn.
move  nro-doc-det-env   to   p-09-doc-pre        ; Documento de Referencia.
move  nCodCli           to   p-09-cod-cli        ; Cขdigo de Cliente
move  cod-pro-det-env   to   p-09-cod-prod       ; Cขdigo de Producto
move  nCntPrd           to   p-09-cnt-prod       ; Cantidad de Producto.

move can-pre-det-env   to nNumero1
move nCntPrd           to nNumero2
perform Iv_Resta_Enteros
move nNumero1          to p-09-cnt-sld

move  cNomcli           to   p-09-nom-cli        ;
move  cDirCli           to   p-09-dir-cli
move  dia-ws            to   p-09-dia
move  mes-ws            to   p-09-mes
move  ano-ws            to   p-09-ano
move  hrs-ws            to   p-09-hora
move  minu-ws           to   p-09-minu
move  sec-ws            to   p-09-segu
move  descri-mae-pro    to   p-09-dsc-prod
move  nNroUnd           to   p-09-und-prod
write REG-PRINT-09
exit.


Dv_Graba.
;--------
perform OPENREF-PRIN-09-F
search first for nNroDoc in PRIN-09-F OFFSET IS p-09-documento
       when EOF exit.
       when ER exit.
       when OK read PRIN-09-F.

perform OPENUP-DEV-ENV-F
MOVE p-09-documento  TO nro-doc-dev           ;
MOVE p-09-doc-pre    TO doc-ref-dev           ; De Prstamo.
MOVE p-09-cod-cli    TO cod-cli-dev
MOVE p-09-cod-prod   TO cod-pro-dev
MOVE p-09-cnt-prod   TO can-pre-dev
move nFecha          to fecha-dev
move nCodVd          to vend-dev
move nRuta           to ruta-dev
move x'0D'           TO cr-dev
move x'0A'           TO lf-dev
write REG-DEV-ENV


perform OPENUP-DET-ENV-F
move p-09-doc-pre  to Documento
move p-09-cod-prod to Producto
search FIRST for Clave_envases in DET-ENV-F OFFSET IS nro-doc-det-env
       when ER  message 1, "Det_env".
       when EOF message 1, "Det_env".
       when OK  read DET-ENV-F
                move p-09-und-prod     to nNroUnd
                move can-pre-det-env   to nNumero1
                move p-09-cnt-prod     to nNumero2
                perform Iv_Resta_Enteros
                move nNumero1          to can-pre-det-env
                if nNumero1 = 0
                   DELETE RECORD FROM DET-ENV-F
                else
                   rewrite  REG-DET-ENV
                end-if
       end-when

move p-09-cod-prod to  cCodPrd
move p-09-cnt-prod to  nCntPrd
move p-09-und-prod to  nNroUnd
perform Iv_Add_Producto
perform Iv_Add_Devolucion
perform Dv_Borra
loop.

Iv_Add_Devolucion.
;-----------------
perform OPENREF-ARC-INV
search FIRST for cCodPrd in ARC-INV-F
       when OK  read ARC-INV-F
                move den-arc-inv     to nNumero1
                move nCntPrd         to nNumero2
                perform Iv_Suma_Enteros
                move nNumero1        to den-arc-inv
                rewrite REG-ARC-INV.
exit.


DV_Borra.
;--------
perform OPENUP-PRIN-09-F
search first for nNroDoc in PRIN-09-F OFFSET IS p-09-documento
       when OK DELETE RECORD FROM PRIN-09-F.
exit.


Dv_Borra_todo.
;-------------
perform Dv_Borra
       when OK loop.
exit.


;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ             REPORTE DE DEVOLUCION DE ENVASES            ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

DV-IMPRIMIR.
;-----------
    perform SETEA-IMPRESORA
            when ER exit.
            when KEND move OK to last-key exit.
    clear.
    message 1, '* IMPRIMIENDO *'.
    next.

DV-IMPRIMIR-DEVOLUCION.
;---------------------
    search FIRST for nNroDoc in PRIN-09-F OFFSET IS p-09-documento
         when ER prompt 1 'Error buscando en spool'  wait 1 second exit.
    read  PRIN-09-F

    build CAB-FACT-000     print.
    build CAB-DV-001.      print.
    build CAB-DV-002.      print.
    perform DV-IMPRIMIR-SIGUIENTE.
    build FIRMA-VEND-CLIENTE print
    build FIN-LISTADO.     print.
    disconnect.                                 ; Termino Comunicaciones IP-40
    exit.


DV-IMPRIMIR-SIGUIENTE.
;---------------------
if p-09-documento =  nNroDoc
   build CAB-DV-003    print
   seek NEXT in PRIN-09-F
     when EOF exit.
   read PRIN-09-F
   loop
end-if
exit.


;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                  PRESTAMOS DE ENVASES                   ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

PRESTAMOS.
;--------
    move 0 to sw-error

    perform LeeHoraFecha
    perform PR-MUEVE-FOLIO                 ; Actualiza documento en nNroDoc.
    perform ShowDocActual                  ; Muestra Informaciขn.
         when ER if lFlgEvCs = 0
                     exit.
         end-when
    move ' ' to cNroDoc
    move 0 to cCodPrd                      ; Borrar Variables.
    move 0 to d-producto-ws
    move 0 to p-producto-ws
    perform PR-ABRIR-MAE-PROD              ; Abre maestro productos.
            when er exit.
    perform PR-ELEGIR-PRODUCTO             ; Rutina Principal.
         when ER exit.

    exit.

PR-ABRIR-MAE-PROD.
;-----------------
  perform OPENREF-MAE-PRO-F
       when ER exit.
  seek first in MAE-PRO-F
    when EOF exit.
  perform Pr_Primer_Producto
  exit.

PR-MUEVE-FOLIO.
;--------------
    move '02' to codigo-tab-fol
    perform LEE-FOLIO
    move folio-tab-fol to nNroDoc

    exit.

PR-ELEGIR-PRODUCTO.
;------------------
    move codigo-mae-pro to cCodPrd        ; Mueve datos.
    move descri-mae-pro to d-producto-ws
    move precio-mae-pro to p-producto-ws
    move nround-mae-pro to nNroUnd


    perform Muestra-Cant-Prod
            when ER exit.
    show P-PRODUCTOS-SD

    perform Pr_Verifica_item
            when OK READ PRIN-08-F
                    move p-08-cnt-prod to nCntPrd
                    show CANT-PROD-SD.

            when ER IF can-producto-ws = 0             ; IF sw-error = 1
                       BEEP
                       MESSAGE 49, 'No hay en Stock '
                    ELSE
                       MESSAGE 49, 'Stock : '
                       SHOW Solo-Hay-Sd
                    END-IF
            end-when


    perform lee-tecla-fast-action
             when ENTER if can-producto-ws = 0  loop.
                        perform Pr_Borra_Item
                        perform PR-ACEPTA-CANTIDAD
                             when ER loop.
                        perform PR-CONFI-PROD
                             when ER loop.
                        perform Pr_Graba_Item
                             when ER loop.
                        perform AVA-PRESTAMO
               end-when
             when DAR   perform AVA-PRESTAMO.
             when UAR   perform RET-PRESTAMO.
             when KEND  perform Pr_Ver_Doc
                                when ER exit.
                        perform PR-CONFI-PR
                                when ER perform Pr_Borra_Todo
                                        exit.
                        perform Pr_Eventual
                        perform PR-IMPRIMIR
                                when OK perform PR-ACTUAL-FOLIO
                                        perform Pr_Graba.
                                when ER perform Pr_Borra_Todo.

                        exit.
             when KELIM perform Pr_Borra_Item.
    loop.

Pr_Borra_Item.
;------------
perform Pr_Verifica_Item
   when OK DELETE RECORD FROM PRIN-08-F.
exit.

Pr_Eventual.
;------------
perform If_Eventual
        when ER exit.
move 1 to punt-ingreso
perform  Acepta_Eventuales
perform OPENUP-EVENTUALE
move nCodCli          to  codcli-eve-ev
move nNroDoc          to  nfac-eve-ev
move cNomEve          to  nombre-eve-ev
move cDirEve          to  direcc-eve-ev
move x'0D'            to  cr-eve-ev
move x'0A'            to  lf-eve-ev
write REG-EVENTUALE

perform OPENREF-PRIN-08-F
search first for nNroDoc in PRIN-08-F
read PRIN-08-F
move cNomCli           to p-08-nom-cli
move cDircli           to p-08-dir-cli
rewrite REG-PRINT-08

exit.



PR-ACEPTA-CANTIDAD.
;------------------
    move 0 to nCntPrd.
    accept CANT-PROD-SD
        when OK  IF nCntPrd = 0 LOOP.
                 move nCntPrd to nCantidad
                 perform Iv_Normal_Entero
                 move nCantidad to nCntPrd
                 perform CHEQUEA-STOCK-ENVASES
                       when ER exit.
                       when OK exit.
        end-when
        when KEND move ER to last-key exit.
        when ER   loop.
    loop.

PR-CONFI-PROD.
;-------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '     Envase     '.
    message 49, '    Yes / No    '.
    action
             when ENTER move OK to last-key exit.
             when KEND  move ER to last-key exit.
             when NO    move ER to last-key exit.
    loop.

PR-CONFI-PR.
;-------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '    Prestamo    '.
    message 49, '    Yes / No    '.
    action
             when ENTER move OK to last-key exit.
             when KEND  move ER to last-key exit.
             when NO    move ER to last-key exit.
    loop.

Pr_Graba_Item.
;-------------
perform OPENUP-PRIN-08-F.
perform Pr_Verifica_Item
    when OK DELETE RECORD FROM PRIN-08-F.

move  nCodCli           to   p-08-cod-cli
move  cNomcli           to   p-08-nom-cli
move  cDirCli           to   p-08-dir-cli
move  nNroDoc           to   p-08-documento
move  dia-ws            to   p-08-dia
move  mes-ws            to   p-08-mes
move  ano-ws            to   p-08-ano
move  hrs-ws            to   p-08-hora
move  minu-ws           to   p-08-minu
move  sec-ws            to   p-08-segu
move  cCodPrd           to   p-08-cod-prod
move  nCntPrd           to   p-08-cnt-prod
move  descri-mae-pro    to   p-08-dsc-prod
move  nNroUnd           to   p-08-und-prod
write REG-PRINT-08

exit.


Pr_Graba.
;--------
perform OPENREF-PRIN-08-F
search first for nNroDoc in PRIN-08-F OFFSET IS p-08-documento
       when EOF exit.
       when ER exit.
       when OK read PRIN-08-F.


  perform OPENUP-CAB-PRE-F                  ; Graba Items
  move p-08-cod-cli      to codigo-cab-pre
  move p-08-documento    to nro-doc-cab-pre
  move p-08-cod-prod     to cod-pro-cab-pre
; move cNroDoc           to doc-csg-cab-pre
  move p-08-cnt-prod     to can-pre-cab-pre
  move nFecha            to fecha-cab-pre
  move nCodVd            to vend-cab-pre
  move nRuta             to ruta-cab-pre
; move 0                 to evcs-cab-pre
  move x'0D'             to cr-cab-pre
  move x'0A'             to lf-cab-pre
  write REG-CAB-PRE

  move p-08-und-prod     to nNroUnd
  move p-08-cod-prod     to cCodPrd
  move p-08-cnt-prod     to nCntPrd
  perform Iv_Sub_Producto
  perform Iv_Add_Prestamos

  perform OPENUP-DET-ENV-F                         ; Aคadir en Envases
  move p-08-cod-cli     to  codigo-det-env         ; Pendientes
  move p-08-documento   to  nro-doc-det-env
  move p-08-cod-prod    to  cod-pro-det-env
  move p-08-cnt-prod    to  can-pre-det-env
  move nFecha           to  fecha-det-env
  write REG-DET-ENV

  perform Pr_Borra
loop.

Iv_Add_Prestamos.
;----------------
perform OPENREF-ARC-INV
search FIRST for cCodPrd in ARC-INV-F
       when OK  read ARC-INV-F
                move pen-arc-inv     to nNumero1
                move nCntPrd         to nNumero2
                perform Iv_Suma_Enteros
                move nNumero1        to pen-arc-inv
                rewrite REG-ARC-INV.
exit.


Pr_Ver_Doc.
;----------
perform OPENUP-PRIN-08-F
search FIRST for nNroDoc in PRIN-08-F
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.

Pr_Verifica_Item.
;----------------
perform OPENUP-PRIN-08-F
move nNroDoc       to Documento
move cCodPrd       to Producto
search first for Clave_Envases in PRIN-08-F
       when EOF move ER  to last-key.
exit.

Pr_Borra.
;--------
perform OPENUP-PRIN-08-F
search first for nNroDoc in PRIN-08-F
       when OK DELETE RECORD FROM PRIN-08-F.
exit.


Pr_Borra_todo.
;-------------
perform Pr_Borra
       when OK loop.
exit.

Pr_Primer_Producto.
;------------------
  move 1  to envase-mae-pro
  search FIRST for envase-mae-pro  in MAE-PRO-F OFFSET IS envase-mae-pro
      when ER exit.
  read MAE-PRO-F
  exit.


AVA-PRESTAMO.
;------------
    seek NEXT in MAE-PRO-F
         when EOF message 49, '-Fin  Productos-'
                  beep
                  wait 1 second
                  perform RET-PRESTAMO.
         when OK  read MAE-PRO-F
                  if envase-mae-pro = 0
                     loop.
                 ;if codigo-mae-pro < 6 OR codigo-mae-pro > 9
                 ;loop.
         end-when
    move OK to last-key
    exit.

RET-PRESTAMO.
;------------
    seek PRIOR in MAE-PRO-F
         when EOF message 49, '-Inicio Product-'
                  beep
                  wait 1 second
                  perform Pr_Primer_Producto.

         when OK  read MAE-PRO-F
                  if envase-mae-pro = 0
                     loop.
                 ;if codigo-mae-pro < 6 OR codigo-mae-pro > 9
                 ;loop.
           end-when
    exit.


CHEQUEA-STOCK-ENVASES.
;---------------------
    perform OPENREF-ARC-INV
            when ER exit.
    seek FIRST in ARC-INV-F
            when EOF exit.
    search FIRST for cCodPrd in ARC-INV-F
            when ER exit.
    read ARC-INV-F

    if canti-arc-inv = 0
       beep
       message 49, ' No queda stock '
       wait 1 seconds
       move ER to last-key
       exit
    end-if
    if nCntPrd > canti-arc-inv
       beep
       message 49, 'Solo Hay : '
       show SOLO-HAY-SD
       wait 1 seconds
       move ER to last-key
       exit
    end-if
    move OK to last-key
    exit.


PR-ACTUAL-FOLIO.
;---------------
    move '02' to codigo-tab-fol
    perform lee-folio

    add 1  to folio-tab-fol.
    rewrite REG-TAB-FOL.
    exit.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ               REPORTE DE PRESTAMO DE ENVASES            ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

PR-IMPRIMIR.
;-----------
    perform SETEA-IMPRESORA
            when ER exit.
            when KEND move OK to last-key exit.
    clear.
    message 1, '* IMPRIMIENDO *'.
    next.

PR-IMPRIMIR-PRESTAMO.
;---------------------
    search FIRST for nNroDoc in PRIN-08-F

    build CAB-FACT-000     print
    build CAB-PR-001       print
    build CAB-PR-002       print
    perform PR-IMPRIMIR-SIGUIENTE
    build FIRMA-VEND-CLIENTE print
    build FIN-LISTADO      print
    disconnect                                  ; Termino Comunicaciones IP-40
    exit.

PR-IMPRIMIR-SIGUIENTE.
;---------------------
read PRIN-08-F
if p-08-documento =  nNroDoc
   build CAB-PR-003    print
   seek NEXT in PRIN-08-F
     when EOF exit.
   loop
end-if
exit.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                 CONSIGNACIONES                          ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
CONSIGNACIONES.
;--------------
 move 1 to punt-pres.

CONSIG001.
;---------
    move 'Consignaciones ' to proc-ws.
    message 1,  '****************'.
    examine punt-pres
            when 0 move 3 to punt-pres      loop.
            when 1 message 17 '    Otorgar      Producto(Csg) '.
            when 2 message 17 '    Recoger      Producto(Csg) '.
      ;     when 3 message 17 ' Recoger Envases               '.
            when 3 message 17 'Devolver Envases               '.
            when 4 move 1 to punt-pres      loop.
    message 33, '    Yes / No    '.
    perform lee-tecla-fast-action
            when ENTER perform CONSIG002
                       exit.
            when KEND  exit.
            when NO    add 1 to punt-pres.
            when FAR   add 1 to punt-pres.
            when BAR   subtract 1 from punt-pres.
    loop.

CONSIG002.
;---------
    examine punt-pres
            when 1 perform OTORGAR.
            when 2 perform RECOGER.
           ;when 3 perform RECOJO-ENV.
            when 3 perform DEVOLVER-ENV.
    exit.


;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                  OTORGAR CONSIGNACION                   ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

OTORGAR.
;------
    move 0 to sw-error

    perform LeeHoraFecha
    perform CS-MUEVE-FOLIO                 ; Actualiza documento en nNroDoc.
    perform ShowDocActual                  ; Muestra Informaciขn.
         when ER exit.
    move 0 to cCodPrd                ; Borrar Variables.
    move 0 to d-producto-ws
    move 0 to p-producto-ws
    move 0 to lFlgEvCs

    perform CS-ABRIR-MAE-PROD              ; Abre maestro productos.
            when er exit.
    perform CS-ELEGIR-PRODUCTO             ; Rutina Principal.
         when ER exit.

    exit.

Cs_Primer_Producto.
;------------------
  move 1  to consig-mae-pro
  search FIRST for consig-mae-pro in MAE-PRO-F OFFSET IS consig-mae-pro
      when ER exit.
  read MAE-PRO-F
  exit.


CS-ABRIR-MAE-PROD.
;-----------------
  perform OPENREF-MAE-PRO-F
       when ER exit.
  seek first in MAE-PRO-F
    when EOF exit.
  perform Cs_Primer_Producto
  exit.

CS-MUEVE-FOLIO.
;--------------
    move '14' to codigo-tab-fol
    perform LEE-FOLIO
    move folio-tab-fol to nNroDoc

    exit.

CS-ELEGIR-PRODUCTO.
;------------------
    move codigo-mae-pro to cCodPrd        ; Mueve datos.
    move descri-mae-pro to d-producto-ws
    move precio-mae-pro to p-producto-ws
    move nround-mae-pro to nNroUnd


    perform Muestra-Cant-Prod
            when ER exit.
    show P-PRODUCTOS-SD

    perform Cs_Verifica_item
            when OK READ PRIN-13-F
                    move p-13-cnt-prod to nCntPrd
                    show CANT-PROD-SD.

            when ER IF can-producto-ws = 0             ; IF sw-error = 1
                       BEEP
                       MESSAGE 49, 'No hay en Stock '
                    ELSE
                       MESSAGE 49, 'Stock : '
                       SHOW Solo-Hay-Sd
                    END-IF
            end-when


    perform lee-tecla-fast-action
             when ENTER if can-producto-ws = 0  loop.
                        perform Cs_Borra_Item
                        perform CS-ACEPTA-CANTIDAD
                             when ER loop.
                        perform CS-CONFI-PROD
                             when ER loop.
                        perform Cs_Graba_Item
                             when ER loop.
                        perform AVA-CONSIGNA
               end-when
             when DAR   perform AVA-CONSIGNA.
             when UAR   perform RET-CONSIGNA.
             when KEND  perform Cs_Ver_Doc
                                when ER exit.
                        perform CS-CONFI-PR
                                when ER perform Cs_Borra_Todo
                                        exit.
                        perform Cs-Actual-Folio
                        perform Cs_Eventual
                        move  0    to      lFlgSinEv
                        move  1    to      lFlgEvCs
                       ;perform Cs_Cnf_Env
                       ;        when ER move 0 to lFlgEvCs.
                       ;        when OK move 1 to lFlgEvCs.

                        perform CS-IMPRIMIR
                                when OK perform Cs_Graba
                                        perform Cs_Graba_Visita
                                        if lFlgEvCs = 0
                                           perform Cs_Cnf_Sin_Env
                                                   when ER move 0 to lFlgSinEv.
                                                   when OK move 1 to lFlgSinEv
                                                           goto  Prestamos
                                                   end-when
                                        end-if
                                end-when
                                when ER perform Cs_Borra_Todo.
                        exit.

             when KELIM perform Cs_Borra_Item.

    loop.


Cs_Graba_Visita.
;---------------
 perform OPENREF-ARC-ATE-F
 seek LAST in ARC-ATE-F
 read ARC-ATE-F
 move    1    to estado-arc-ate
 move    1    to venta-arc-ate
 rewrite REG-ARC-ATE
 exit.

;Cs_Imp_Envases.
;;--------------
;    move nNroDoc  to cNroDoc
;    perform PR-MUEVE-FOLIO                 ; Actualiza documento en nNroDoc.
;    perform Pr_Ver_Doc
;            when ER exit.
;    perform PR-CONFI-PR
;            when ER perform Pr_Borra_Todo
;                    exit.
;    perform Pr_Eventual
;    perform PR-IMPRIMIR
;            when OK perform PR-ACTUAL-FOLIO
;                    perform Pr_Graba.
;            when ER perform Pr_Borra_Todo.
;exit.



Cs_Cnf_Env.
;----------
    message  1, '****************'.
    message 17, '  Consignacion  '.
    message 33, '  Con Envases?  '.
    message 49, '    Yes / No    '.
    action
             when ENTER move OK to last-key exit.
             when KEND  move ER to last-key exit.
             when NO    move ER to last-key exit.
    loop.

Cs_Cnf_Sin_Env.
;----------
    message  1, '****************'.
    message 17, '  Consignacion  '.
    message 33, ' con Prestamo ? '.
    message 49, '    Yes / No    '.
    action
             when ENTER move OK to last-key exit.
             when KEND  move ER to last-key exit.
             when NO    move ER to last-key exit.
    loop.




Cs_Borra_Item.
;-------------
perform Cs_Verifica_Item
   when OK DELETE RECORD FROM PRIN-13-F.
exit.

Cs_Eventual.
;------------
perform If_Eventual
        when ER exit.
move 1 to punt-ingreso
perform  Acepta_Eventuales
perform OPENUP-EVENTUALC
move nCodCli          to  codcli-eve-cs
move nNroDoc          to  nfac-eve-cs
move cNomEve          to  nombre-eve-cs
move cDirEve          to  direcc-eve-cs
move x'0D'            to  cr-eve-cs
move x'0A'            to  lf-eve-cs

write REG-EVENTUALC
exit.



CS-ACEPTA-CANTIDAD.
;------------------
    move 0 to nCntPrd.
    accept CANT-PROD-SD
        when OK  IF nCntPrd = 0 LOOP.
                 move nCntPrd to nCantidad
                 perform Iv_Normal_Entero
                 move nCantidad to nCntPrd
                 perform CHEQUEA-STOCK-CONSIGNA
                       when ER exit.
                       when OK exit.
        end-when
        when KEND move ER to last-key exit.
        when ER   loop.
    loop.

CS-CONFI-PROD.
;-------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '    Producto    '.
    message 49, '    Yes / No    '.
    action
             when ENTER move OK to last-key exit.
             when KEND  move ER to last-key exit.
             when NO    move ER to last-key exit.
    loop.

CS-CONFI-PR.
;-------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '  Consignacion  '.
    message 49, '    Yes / No    '.
    action
             when ENTER move OK to last-key exit.
             when KEND  move ER to last-key exit.
             when NO    move ER to last-key exit.
    loop.

Cs_Graba_Item.
;-------------
perform OPENUP-PRIN-13-F.
perform Cs_Verifica_Item
    when OK DELETE RECORD FROM PRIN-13-F.

move  nCodCli           to   p-13-cod-cli
move  cNomcli           to   p-13-nom-cli
move  cDirCli           to   p-13-dir-cli
move  nNroDoc           to   p-13-documento
move  dia-ws            to   p-13-dia
move  mes-ws            to   p-13-mes
move  ano-ws            to   p-13-ano
move  hrs-ws            to   p-13-hora
move  minu-ws           to   p-13-minu
move  sec-ws            to   p-13-segu
move  cCodPrd           to   p-13-cod-prod
move  nCntPrd           to   p-13-cnt-prod
move  descri-mae-pro    to   p-13-dsc-prod
move  nNroUnd           to   p-13-und-prod
write REG-PRINT-13

exit.

Cs_Graba.
;--------
perform OPENREF-PRIN-13-F
search first for nNroDoc in PRIN-13-F OFFSET IS p-13-documento
       when EOF exit.
       when ER exit.
       when OK read PRIN-13-F.


  perform OPENUP-CAB-CSG-F                  ; Graba Items
  move p-13-cod-cli      to codigo-cab-csg
  move p-13-documento    to nro-doc-cab-csg
  move p-13-cod-prod     to cod-pro-cab-csg
  move p-13-cnt-prod     to can-pre-cab-csg
  move nFecha            to fecha-cab-csg
  move nCodVd            to vend-cab-csg
  move nRuta             to ruta-cab-csg
  move lFlgEvCs          to Env-cab-csg
  move x'0D'             to cr-cab-csg
  move x'0A'             to lf-cab-csg
  write REG-CAB-CSG

  perform OPENUP-DET-CSG-F                         ; Aคadir en Consignaciones
  move p-13-cod-cli     to  codigo-det-csg         ; Pendientes
  move p-13-documento   to  nro-doc-det-csg
  move p-13-cod-prod    to  cod-pro-det-csg
  move p-13-cnt-prod    to  can-pre-det-csg
  move nFecha           to  fecha-det-csg
  move lFlgEvCs         to  Env-det-csg
  write REG-DET-CSG


  move p-13-und-prod     to nNroUnd
  move p-13-cod-prod     to cCodPrd
  move p-13-cnt-prod     to nCntPrd
  perform Iv_Sub_Producto
  perform Iv_Add_Consigna

  if lFlgEvCs = 0            ; Sin Envases.
     perform Cs_Gen_Prd_Rel  ; Aumenta al Inventario.
     move p-13-cod-prod   to cCodPrd
     move p-13-und-prod   to nNroUnd
     move p-13-cnt-prod   to nCntPrd
     move p-13-cnt-prod   to p-02-can-prod
     perform Fc_Cs_Envases                       ; Busca si tiene envases
  end-if
  perform Cs_Borra
loop.





Cs_Gen_Prd_Rel.
;--------------
; Actualizar las relaciones de productos (CPG y CPC)

perform OPENREF-MAE-PRO-F
search FIRST for cCodPrd   in MAE-PRO-F
          when ER exit.
read MAE-PRO-F
move  relac1-mae-pro to nRelEnv1
move  relac2-mae-pro to nRelEnv2

if nRelEnv1 > 0 or nRelEnv2 > 0
   if nRelEnv1 > 0
      search FIRST for nRelEnv1 in MAE-PRO-F
             when OK read MAE-PRO-F
                     move nround-mae-pro to nNroUnd.

       move cCodPrd  to busca-pro-ws
       move nRelEnv1 to cCodPrd
       perform Iv_Add_Producto
       move busca-pro-ws to cCodPrd

   ; if lFlgEvCs = 0                    ; Sin Envases
   ;   move cCodPrd  to busca-pro-ws
   ;   move nRelEnv1 to cCodPrd
   ;   perform Cs_Graba_Prd_Rel
   ;   move busca-pro-ws to cCodPrd
   ; end-if
   end-if

   if nRelEnv2 > 0
      search FIRST for nRelEnv2 in MAE-PRO-F
             when OK read MAE-PRO-F
                     move nround-mae-pro to nNroUnd.

        move cCodPrd  to busca-pro-ws
        move nRelEnv2 to cCodPrd
        perform Iv_Add_Producto
        move busca-pro-ws to cCodPrd

   ; if lFlgEvCs = 0                    ; Sin Envases ?
   ;    move cCodPrd  to busca-pro-ws
   ;    move nRelEnv2 to cCodPrd
   ;    perform Cs_Graba_Prd_Rel
   ;    move busca-pro-ws to cCodPrd
   ; end-if
   end-if
search FIRST for cCodPrd   in MAE-PRO-F
       when ER exit.
read MAE-PRO-F
end-if
move OK to last-key
exit.


Iv_Add_Consigna.
;----------------
perform OPENREF-ARC-INV
search FIRST for cCodPrd in ARC-INV-F
       when OK  read ARC-INV-F
                move cso-arc-inv     to nNumero1
                move nCntPrd         to nNumero2
                perform Iv_Suma_Enteros
                move nNumero1        to cso-arc-inv
                rewrite REG-ARC-INV.
exit.


Cs_Ver_Doc.
;----------
perform OPENUP-PRIN-13-F
search FIRST for nNroDoc in PRIN-13-F
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.

Cs_Verifica_Item.
;----------------
perform OPENUP-PRIN-13-F
move nNroDoc       to Documento
move cCodPrd       to Producto
search first for Clave_Envases in PRIN-13-F
       when EOF move ER  to last-key.
exit.

Cs_Borra.
;--------
perform OPENUP-PRIN-13-F
search first for nNroDoc in PRIN-13-F
       when OK DELETE RECORD FROM PRIN-13-F.
exit.


Cs_Borra_todo.
;-------------
perform Cs_Borra
       when OK loop.
exit.

AVA-CONSIGNA.
;------------
    seek NEXT in MAE-PRO-F
         when EOF message 49, '-Final  archivo-'
                  beep
                  wait 1 second
                  perform RET-CONSIGNA.
         when OK  read MAE-PRO-F
                  if consig-mae-pro = 0 loop.
         end-when
    move OK to last-key
    exit.

RET-CONSIGNA.
;------------
    seek PRIOR in MAE-PRO-F
         when EOF message 49, '-Inicio archivo-'
                  beep
                  wait 1 second
                  perform Cs_Primer_Producto.
         when OK  read MAE-PRO-F
                  if consig-mae-pro = 0 loop.
                 ;if codigo-mae-pro < 6 OR codigo-mae-pro > 9
                 ;loop.
           end-when
    exit.


CHEQUEA-STOCK-CONSIGNA.
;---------------------
    perform OPENREF-ARC-INV
            when ER exit.
    seek FIRST in ARC-INV-F
            when EOF exit.
    search FIRST for cCodPrd in ARC-INV-F
            when ER exit.
    read ARC-INV-F

    if canti-arc-inv = 0
       beep
       message 49, ' No queda stock '
       wait 1 seconds
       move ER to last-key
       exit
    end-if
    if nCntPrd > canti-arc-inv
       beep
       message 49, 'Solo Hay : '
       show SOLO-HAY-SD
       wait 1 seconds
       move ER to last-key
       exit
    end-if
    move OK to last-key
    exit.


CS-ACTUAL-FOLIO.
;---------------
    move '14' to codigo-tab-fol
    perform lee-folio

    add 1  to folio-tab-fol.
    rewrite REG-TAB-FOL.
    exit.


;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ               REPORTE DE CONSIGNACIONES OTORGADAS       ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

CS-IMPRIMIR.
;-----------
    perform SETEA-IMPRESORA
            when ER exit.
            when KEND move OK to last-key exit.

    clear
    message 1, '* IMPRIMIENDO *'.
    next.

CS-IMPRIMIR-PRESTAMO.
;---------------------
    search FIRST for nNroDoc in PRIN-13-F

    build CAB-FACT-000     print
    if lFlgEvCs = 0
        build CAB-CS-001       print
    else
        build CAB-CS-001-1     print.

    build CAB-CS-002       print
;   build CAB-CS-004       print
    perform CS-IMPRIMIR-SIGUIENTE
    build FIRMA-VEND-CLIENTE print
    build FIN-LISTADO      print
    disconnect                                  ; Termino Comunicaciones IP-40
    exit.

CS-IMPRIMIR-SIGUIENTE.
;---------------------
read PRIN-13-F
if p-13-documento =  nNroDoc
   build CAB-CS-003    print
   seek NEXT in PRIN-13-F
     when EOF exit.
   loop
end-if
exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                     Recojo de Envases de Consignaciขn                  ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

Recojo-Env.
;----------
 perform Rj_Ver_Env
         when ER exit.
 move cod-pro-env-csg to busca-pro-ws      ; Busca producto.
 perform BUSCA-COD-PROD
      when ER exit.

 perform Rj_MUEVE-FOLIO                    ; 16 = Recojo de Envases.
 perform ShowDocActual
      when ER exit.
 perform Rj_Muestra
      when ER exit.
    exit.


Rj_MUEVE-FOLIO.
;--------------
    move '16' to codigo-tab-fol
    perform LEE-FOLIO
    exit.


Rj_Ver_Env.
;----------
    perform OPENREF-ENV-CSG-F
         when ER exit.
    seek first in ENV-CSG-F
      when EOF exit.
    search FIRST for nCodCli           in ENV-CSG-F
           when Ok read ENV-CSG-F.
      ;            move nro-doc-env-csg to cNroDoc.
exit.

Rj_Cnf_Env.
;-------------
    message  1, '****************'.
    message 17, 'Ingresar envases'.
    message 33, ' por Adelantado '.
    message 49, '    Yes / No    '.
    action
             when ENTER move OK to last-key exit.
             when KEND  move ER to last-key exit.
             when NO    move ER to last-key exit.
    loop.


Rj_Muestra.
;----------
;   move nro-doc-env-csg to cNroDoc           ; Print-18 es el Archivo
    move cod-pro-env-csg to cCodPrd           ; Generado.
    move nround-mae-pro  to nNroUnd
    move descri-mae-pro  to d-producto-ws
    move can-pre-env-csg to can-producto-ws
    move fecha-det-env   to fecha-emisi-ws

    move can-producto-ws to nStock

    show ENVASES-DOS-SD
    perform Rj_Verifica_item
            when OK READ PRIN-17-F                      ; Recojo de Envases.
                    move p-17-cnt-prod     to nCntPrd
                    move nStock            to nNumero1
                    move p-17-cnt-prod     to nNumero2
                    perform Iv_Resta_Enteros
                    move nNumero1          to nStock
                    show ENVASES-TRES-SD
                    show CANT-PRODUCTO-SD.
            when ER show ENVASES-TRES-SD
                    perform Rj_MUESTRA-DOCUMENTO.

    perform lee-tecla-action
        when ENTER if can-producto-ws > 0
                      perform Rj_Borra_Item
                      perform Rj_Acepta_Cantidad
                              when ER loop.
                      perform Rj_Confirma_Item      ; Confirma Item ?
                              when ER loop.
                      perform Rj_Graba_Spool
                      perform Rj_AVANZA-PRODUCTO
                   else
                      beep
                   end-if
        end-when
        when DAR   perform Rj_AVANZA-PRODUCTO.
        when UAR   perform Rj_ANTERIOR-PRODUCTO.
        when FAR   perform Rj_MUESTRA-FEC-DOC.
        when KEND  perform Rj_Ver_Doc
                       when ER exit.
                   perform Rj_CONFI-DV
                       when ER perform Rj_Borra_Todo exit.
                   perform Rj_IMPRIMIR
                           when OK perform Rj_Graba
                                   perform Rj_ACTUALIZA-FOLIO.

                           when ER perform Rj_Borra_Todo.
                   exit.
        when KELIM perform Rj_Borra_Item.
      loop.

Rj_ACTUALIZA-FOLIO.
;--------------
  move '16' to ws-cod-folio              ; 03 para DEVOLUCIONES.
  search FIRST for ws-cod-folio in TAB-FOL-F
  add 1  to folio-tab-fol
  rewrite REG-TAB-FOL
  exit.


Rj_Borra_Item.
;-------------
perform Rj_Verifica_item
   when OK DELETE RECORD FROM PRIN-17-F.
exit.

Rj_Ver_Doc.
;----------
perform OPENUP-PRIN-17-F
search FIRST for nNroDoc in PRIN-17-F OFFSET IS p-17-documento
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.

Rj_Verifica_Item.
;----------------
perform OPENUP-PRIN-17-F
move nNroDoc       to Documento
move cCodPrd       to Producto
search first for Clave_Envases in PRIN-17-F
       when EOF move ER  to last-key.
exit.


Rj_BUSCA-DEVOLUCION.
;-------------------
search FIRST for nNroDoc in DEV-ENV-F
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.


Rj_MUESTRA-DOCUMENTO.
;--------------------
if can-producto-ws <= 0
   message 49, '  Ya devueltos  '
   beep
   wait 1 second
end-if
show DV-SHOW-DOC
exit.

Rj_MUESTRA-FEC-DOC.
;------------------
show ENVASES-UNO-SD
action
     when enter move OK to last-key
                exit.
     when KEND exit.
loop.


Rj_primer-producto.
;-----------------
   search first for nCodCli           in ENV-CSG-F
   when er move eof to last-key.
   when ok read ENV-CSG-F
           move cod-pro-env-csg to busca-pro-ws
           perform BUSCA-COD-PROD
                   when ER exit.
   end-when
exit.

Rj_AVANZA-PRODUCTO.
;------------------
search next for nCodCli           in ENV-CSG-F
when ER message 49, '-Fin de archivo-'
        beep
        wait 1 second
        perform Rj_ultimo_producto.

when OK read ENV-CSG-F
        move cod-pro-env-csg to busca-pro-ws
        perform BUSCA-COD-PROD
                when ER exit.
        end-when
exit.

Rj_ultimo_producto.
;------------------
search first for Clave_Envases in ENV-CSG-F
       when EOF move ER  to last-key.
exit.


Rj_ANTERIOR-PRODUCTO.
;---------------------
   perform Seek-Prior-env-csg until nCodCli = codigo-env-csg or last-key = eof
   if last-key = ok
      move cod-pro-env-csg to busca-pro-ws
      perform BUSCA-COD-PROD
              when ER exit.
   else
      message 49, '-Ini de archivo-'
      beep
      wait 1 second
      perform Rj_primer-producto
      move eof to last-key
   end-if
exit.

Seek-Prior-env-csg.
;------------------
   seek prior in ENV-CSG-F
   when ok read ENV-CSG-F.
EXIT.



Rj_Acepta_Cantidad.
;------------------
    move  can-producto-ws to nCntPrd
    if nCntPrd <= 0
       exit.
    move 0  to nCntPrd
    accept CANT-PRODUCTO-SD
        when OK    if nCntPrd = 0 loop.
                   move nCntPrd to nCantidad
                   perform Iv_Normal_Entero
                   move nCantidad to nCntPrd

                   if nCntPrd > can-producto-ws
                      beep
                      message 49, ' Cantidad Mayor '
                      wait 1 seconds
                      move ER to last-key
                   end-if
                   exit
        end-when
        when KEND  move ER TO last-key  exit.
    loop.


Rj_CONFI-DV.
;-------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '    Devolucion  '.
    message 49, '    Yes / No    '.
    action
             when ENTER move 1 to nAtencion
                        move OK to last-key exit.

             when KEND  move ER to last-key exit.
             when NO    move ER to last-key exit.
    loop.

Rj_Confirma_Item.
;----------------
    message  1, '****************'.
    message 17, 'Confirma Ingreso'.
    message 33, '    Yes / No    '.
    message 49, '****************'.
    perform lee-tecla-action
        when ENTER exit.
        when NO    move ER to last-key exit.
    loop.


Rj_Graba_Spool.
;--------------
perform OPENUP-PRIN-17-F

move  nNroDoc           to   p-17-documento      ; Documento de la Devoluciขn.
;move  cNroDoc          to   p-17-doc-ref        ; Documento de Referencia.
move  nCodCli           to   p-17-cod-cli        ; Cขdigo de Cliente
move  cod-pro-env-csg   to   p-17-cod-prod       ; Cขdigo de Producto
move  nCntPrd           to   p-17-cnt-prod       ; Cantidad de Producto.

move  cNomcli           to   p-17-nom-cli        ;
move  cDirCli           to   p-17-dir-cli
move  nFecha            to   p-17-date
move  descri-mae-pro    to   p-17-dsc-prod
move  nNroUnd           to   p-17-und-prod
write REG-PRINT-17
exit.


Rj_Graba.
;--------
perform OPENREF-PRIN-17-F
search first for nNroDoc in PRIN-17-F OFFSET IS p-17-documento
       when EOF exit.
       when ER exit.
       when OK read PRIN-17-F.

perform OPENUP-PrEvCsg-F
move p-17-documento  to nro-doc-csg-pre
move p-17-cod-cli    to codigo-csg-pre
;move p-17-doc-ref    to nro-doc-csg-pre
move p-17-cod-prod   to cod-pro-csg-pre
move p-17-cnt-prod   to can-pre-csg-pre
move nFecha          to fecha-csg-pre
move nRuta           to ruta-csg-pre
move nCodVd          to vend-csg-pre
move x'0D'           to cr-csg-pre
move x'0A'           to lf-csg-pre

write REG-PrEvCsg

perform OPENUP-ENV-ADE-F
move p-17-documento  to nro-doc-env-ade
move p-17-cod-cli    to codigo-env-ade
move p-17-cod-prod   to cod-pro-env-ade
move p-17-cnt-prod   to can-pre-env-ade
write REG-ENV-ADE

perform OPENUP-ENV-CSG-F
move nCodCli         to Cliente
move p-17-cod-prod   to ProductoCsg
search FIRST for Clave_consignacion ENV-CSG-F
       when OK  read ENV-CSG-F
                move can-pre-env-csg   to nNumero1
                move p-17-cnt-prod     to nNumero2
                perform Iv_Resta_Enteros
                move nNumero1          to can-pre-env-csg
                if nNumero1 = 0
                   DELETE RECORD FROM ENV-CSG-F
                else
                   rewrite  REG-ENV-CSG
                end-if
       end-when

move p-17-cod-prod to  cCodPrd
move p-17-cnt-prod to  nCntPrd
move p-17-und-prod to  nNroUnd
perform Iv_Add_Producto
perform Rj_Borra
loop.

Rj_Borra.
;--------
perform OPENUP-PRIN-17-F
search first for nNroDoc in PRIN-17-F OFFSET IS p-17-documento
       when OK DELETE RECORD FROM PRIN-17-F.
exit.


Rj_Borra_todo.
;-------------
perform Rj_Borra
       when OK loop.
exit.

Rj_Imprimir.
;-----------
move Ok to last-key
exit.


;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                     Devolucion de Consiganciones.                      ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

RECOGER.
;------
    PERFORM LeeHoraFecha                       ; Lee Hora y Fecha en Variables.

    perform OPENREF-DET-CSG-F.
         when ER exit.
    seek first in DET-CSG-F.
      when EOF exit.
    search FIRST for nCodCli           in DET-CSG-F
      when ER  beep
               message  1, '****************'
               message 17, 'Cliente NO  Debe'
               message 33, ' Consignaciones '
               message 49, '****************'
               wait 1 seconds.

      when OK  read DET-CSG-F
               move env-det-csg     to lFlgEvCs
               move cod-pro-det-csg to busca-pro-ws
               perform BUSCA-COD-PROD
                    when ER exit.
               perform CSDV-MUEVE-FOLIO
             ; PERFORM ShowDocActual
             ;      when ER exit.
               perform CsDv_Muestra
                    when ER exit.
      end-when

    exit.

CSDV-MUEVE-FOLIO.
;--------------
    move '15' to codigo-tab-fol
    perform LEE-FOLIO
    exit.

CsDv_Muestra.
;----------
    move nro-doc-det-csg to cNroDoc
    move cod-pro-det-csg to cCodPrd
    move nround-mae-pro  to nNroUnd
    move descri-mae-pro  to d-producto-ws
    move can-pre-det-csg to can-producto-ws
    move fecha-det-csg   to fecha-emisi-ws

    move can-producto-ws to nStock

    show ENVASES-DOS-SD
    perform CsDv_Verifica_item
            when OK READ PRIN-15-F
                    move p-15-cnt-prod to nCntPrd
                    move nStock            to nNumero1
                    move p-15-cnt-prod     to nNumero2
                    perform Iv_Resta_Enteros
                    move nNumero1          to nStock
                    show ENVASES-TRES-SD
                    show CANT-PRODUCTO-SD.

            when ER show ENVASES-TRES-SD
                    perform CSDV-MUESTRA-DOCUMENTO.
    perform lee-tecla-action
        when ENTER if can-producto-ws > 0
                      perform CsDv_Borra_Item
                      perform CsDv_Acepta_Cantidad
                              when ER loop.
                      perform CsDv_Confirma_Item      ; Confirma Item ?
                              when ER loop.
                      perform CsDv_Graba_Spool
                      perform CSDV-AVANZA-PRODUCTO
                   else
                      beep
                   end-if
        end-when
        when DAR   perform CSDV-AVANZA-PRODUCTO.
        when UAR   perform CSDV-ANTERIOR-PRODUCTO.
        when FAR   perform CSDV-MUESTRA-FEC-DOC.
        when KEND  perform CsDv_Ver_Doc
                       when ER exit.
                   perform CSDV-CONFI-DV
                       when ER perform CsDv_Borra_Todo exit.
                   perform CSDV-IMPRIMIR
                           when OK perform CsDv_Graba
                                   perform CSDV-ACTUALIZA-FOLIO.
                           when ER perform CsDv_Borra_Todo.
                   exit.
        when KELIM perform CsDv_Borra_Item.

      loop.

CsDv_Borra_Item.
;---------------
perform CsDv_Verifica_item
   when OK DELETE RECORD FROM PRIN-15-F.
exit.

CsDv_Ver_Doc.
;----------
perform OPENUP-PRIN-15-F
search FIRST for nNroDoc in PRIN-15-F OFFSET IS p-15-documento
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.


CsDv_Verifica_Item.
;----------------
perform OPENUP-PRIN-15-F
move cNroDoc       to Documento
move cCodPrd       to Producto
search first for Clave_Envases in PRIN-15-F
       when EOF move ER  to last-key.
exit.

CSDV-BUSCA-DEVOLUCION.
;-------------------
search FIRST for nNroDoc in DEV-CSG-F
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.


CSDV-MUESTRA-DOCUMENTO.
;--------------------
if can-producto-ws <= 0
   message 49, '  Ya devueltos  '
   beep
   wait 1 second
end-if
show DV-SHOW-DOC
exit.

CSDV-MUESTRA-FEC-DOC.
;------------------
show ENVASES-UNO-SD
action
     when enter move OK to last-key
                exit.
     when KEND exit.
loop.


SEEK-PRIOR-DET-CSG.
;-----------------
   seek prior in DET-CSG-F
   when ok read DET-CSG-F.
EXIT.

SEEK-PRIOR-PEDIDOS.
;-----------------
   seek prior in PEDIDOS-F
   when ok read PEDIDOS-F.
EXIT.


CSDV-primer-producto.
;-----------------
   search first for nCodCli           in DET-CSG-F
   when er move eof to last-key.
   when ok read DET-CSG-F
           move cod-pro-det-csg to busca-pro-ws
           perform BUSCA-COD-PROD
                   when ER exit.
   end-when
exit.

PED-primer-producto.
;-----------------
   search first for nCodCli           in PEDIDOS-F
   when er move eof to last-key.
   when ok read PEDIDOS-F
           move cod-pro-ped to busca-pro-ws
           perform BUSCA-COD-PROD
                   when ER exit.
   end-when
exit.

CSDV-AVANZA-PRODUCTO.
;------------------
search next for nCodCli           in DET-CSG-F
when er message 49, '-Fin de archivo-'
        beep
        wait 1 second
        perform CsDv_ultimo_producto.

when ok read DET-CSG-F
        move cod-pro-det-csg to busca-pro-ws
        perform BUSCA-COD-PROD
                when ER exit.
        end-when
exit.

CsDv_ultimo_producto.
;------------------
search first for Clave_Envases in DET-CSG-F
       when EOF move ER  to last-key.
exit.


PED-AVANZA-PRODUCTO.
;------------------
search next for nCodCli           in PEDIDOS-F
when ER message 49, '-Fin de archivo-'
        beep
        wait 1 second
        perform Ped_ultimo_producto.

when OK read PEDIDOS-F
        move cod-pro-ped     to busca-pro-ws
        perform BUSCA-COD-PROD
                when ER exit.
        end-when
exit.

Ped_ultimo_producto.
;------------------
search first for Clave_Envases in PEDIDOS-F
       when EOF move ER  to last-key.
exit.



CSDV-ANTERIOR-PRODUCTO.
;---------------------
   perform seek-prior-det-csg until nCodCli = codigo-det-csg or last-key = eof
   if last-key = ok
      move cod-pro-det-csg to busca-pro-ws
      perform BUSCA-COD-PROD
              when ER exit.
   else
      message 49, '-Ini de archivo-'
      beep
      wait 1 second
      perform CSDV-primer-producto
      move eof to last-key
   end-if
exit.



PED-ANTERIOR-PRODUCTO.
;---------------------
   perform seek-prior-pedidos until nCodCli = codigo-ped or last-key = eof
   if last-key = ok
      move cod-pro-ped to busca-pro-ws
      perform BUSCA-COD-PROD
              when ER exit.
   else
      message 49, '-Ini de archivo-'
      beep
      wait 1 second
      perform PED-primer-producto
      move eof to last-key
   end-if
exit.



CsDv_Acepta_Cantidad.
;------------------
    move  can-producto-ws to nCntPrd
    if nCntPrd <= 0
       exit.
    move 0  to nCntPrd
    accept CANT-PRODUCTO-SD
        when OK    if nCntPrd = 0 loop.
                   move nCntPrd to nCantidad
                   perform Iv_Normal_Entero
                   move nCantidad to nCntPrd

                   if nCntPrd > can-producto-ws
                      beep
                      message 49, ' Cantidad Mayor '
                      wait 1 seconds
                      move ER to last-key
                   end-if
                   exit
        end-when
        when KEND   move ER to last-key exit.
    loop.


CSDV-CONFI-DV.
;-------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '    Devolucion  '.
    message 49, '    Yes / No    '.
    action
             when ENTER move 1 to nAtencion
                        move OK to last-key exit.

             when KEND  move ER to last-key exit.
             when NO    move ER to last-key exit.
    loop.

CSDV-ACTUALIZA-FOLIO.
;--------------
    move '15' to ws-cod-folio              ; 03 para DEVOLUCIONES.
    search FIRST for ws-cod-folio in TAB-FOL-F
    READ TAB-FOL-F
    add 1  to folio-tab-fol
    rewrite REG-TAB-FOL
    exit.

CsDv_Confirma_Item.
;----------------
    message  1, '****************'.
    message 17, 'Confirma Ingreso'.
    message 33, '    Yes / No    '.
    message 49, '****************'.
    perform lee-tecla-action
        when ENTER            exit.
        when NO    move ER to last-key exit.
    loop.


CsDv_Graba_Spool.
;--------------
perform OPENUP-PRIN-15-F

move  nNroDoc           to   p-15-documento      ; Documento de la Devoluciขn.
move  nro-doc-det-csg   to   p-15-doc-pre        ; Documento de Referencia.
move  nCodCli           to   p-15-cod-cli        ; Cขdigo de Cliente
move  cod-pro-det-csg   to   p-15-cod-prod       ; Cขdigo de Producto
move  nCntPrd           to   p-15-cnt-prod       ; Cantidad de Producto.

move  cNomcli           to   p-15-nom-cli        ;
move  cDirCli           to   p-15-dir-cli
move  dia-ws            to   p-15-dia
move  mes-ws            to   p-15-mes
move  ano-ws            to   p-15-ano
move  hrs-ws            to   p-15-hora
move  minu-ws           to   p-15-minu
move  sec-ws            to   p-15-segu
move  descri-mae-pro    to   p-15-dsc-prod
move  nNroUnd           to   p-15-und-prod
write REG-PRINT-15
exit.


CsDv_Graba.
;--------
perform OPENREF-PRIN-15-F
search first for nNroDoc in PRIN-15-F OFFSET IS p-15-documento
       when EOF exit.
       when ER exit.
       when OK read PRIN-15-F.

perform OPENUP-DEV-CSG-F
move p-15-documento  to nro-doc-csg           ;
move p-15-doc-pre    to doc-ref-csg           ; De Prstamo.
move p-15-cod-cli    to cod-cli-csg
move p-15-cod-prod   to cod-pro-csg
move p-15-cnt-prod   to can-pre-csg
move nFecha          to fecha-csg
move nCodVd          to vend-csg
move nRuta           to ruta-csg
move x'0D'           TO cr-csg
move x'0A'           TO lf-csg
write REG-DEV-CSG


perform OPENUP-DET-CSG-F
move p-15-doc-pre  to Documento
move p-15-cod-prod to Producto
search FIRST for Clave_envases in DET-CSG-F OFFSET IS nro-doc-det-csg
       when OK  read DET-CSG-F
                move can-pre-det-csg   to nNumero1
                move p-15-cnt-prod     to nNumero2
                perform Iv_Resta_Enteros
                move nNumero1          to can-pre-det-csg
                if nNumero1 = 0
                   DELETE RECORD FROM DET-CSG-F
                else
                   rewrite  REG-DET-CSG
                end-if
       end-when

if lFlgEvCs = 0                 ; Sin Envases.
   move p-15-cod-prod to  cCodPrd
   move p-15-cnt-prod to  nCntPrd
   move p-15-und-prod to  nNroUnd
   perform Cs_Add_Prd_Rel
end-if

move p-15-cod-prod to  cCodPrd
move p-15-cnt-prod to  nCntPrd
move p-15-und-prod to  nNroUnd
perform Iv_Add_Producto
perform Iv_Add_DevCs
perform CsDv_Borra
loop.


Cs_Add_Prd_Rel.
;--------------
; Actualizar las relaciones de productos (CPG y CPC)

perform OPENREF-MAE-PRO-F
search FIRST for cCodPrd   in MAE-PRO-F
          when ER exit.
read MAE-PRO-F
move  relac1-mae-pro to nRelEnv1
move  relac2-mae-pro to nRelEnv2

if nRelEnv1 > 0 or nRelEnv2 > 0
   if nRelEnv1 > 0
      search FIRST for nRelEnv1 in MAE-PRO-F
             when OK read MAE-PRO-F.

      move nRelEnv1 to cCodPrd
      move nround-mae-pro  to  nNroUnd
      move p-15-cnt-prod to  nCntPrd
      perform Cs_Graba_Prd_Rel

   end-if

   if nRelEnv2 > 0
      search FIRST for nRelEnv2 in MAE-PRO-F
             when OK read MAE-PRO-F.

      move nRelEnv2 to cCodPrd
      move nround-mae-pro  to  nNroUnd
      move p-15-cnt-prod to  nCntPrd
      perform Cs_Graba_Prd_Rel
   end-if
   search FIRST for p-15-cod-prod in MAE-PRO-F
          when ER exit.
   read MAE-PRO-F
end-if
move OK to last-key
exit.

Cs_Graba_Prd_Rel.
;------------------
      perform Verifica_Env-Csg
            when OK READ Env-Csg-F
                    move can-pre-env-csg   to nNumero1
                    move nCntPrd           to nNumero2
                    perform Iv_Suma_Enteros
                    move  nNumero1         to can-pre-env-csg
                    rewrite REG-ENV-CSG.
            when ER move nCodCli  to codigo-env-csg
                    move cCodPrd  to cod-pro-env-csg
                    move p-15-cnt-prod to can-pre-env-csg
                    move nFecha        to fecha-env-csg
                    write REG-ENV-CSG.
exit.



Iv_Add_DevCs.
;-----------------
perform OPENREF-ARC-INV
search FIRST for cCodPrd in ARC-INV-F
       when OK  read ARC-INV-F
                move csr-arc-inv     to nNumero1
                move nCntPrd         to nNumero2
                perform Iv_Suma_Enteros
                move nNumero1        to csr-arc-inv
                rewrite REG-ARC-INV.
exit.


CsDv_Borra.
;--------
perform OPENUP-PRIN-15-F
search first for nNroDoc in PRIN-15-F OFFSET IS p-15-documento
       when OK DELETE RECORD FROM PRIN-15-F.
exit.


CsDv_Borra_todo.
;-------------
perform Dv_Borra
       when OK loop.
exit.


;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ             REPORTE DE DEVOLUCION DE CONSIGNACION       ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

CSDV-IMPRIMIR.
;-----------
    perform SETEA-IMPRESORA
            when ER exit.
            when KEND move OK to last-key exit.
    clear.
    message 1, '* IMPRIMIENDO *'.
    next.

CSDV-IMPRIMIR-DEVOLUCION.
;---------------------
    search FIRST for nNroDoc in PRIN-15-F OFFSET IS p-15-documento
    read  PRIN-15-F

    build CAB-FACT-000     print.
    if lFlgEvCs = 0
       build CAB-CSDV-001       print
    else
       build CAB-CSDV-001-1     print.

    build CAB-CSDV-002.      print.
    perform CSDV-IMPRIMIR-SIGUIENTE.
    build FIRMA-VEND-CLIENTE print
    build FIN-LISTADO.     print.
    disconnect.                                 ; Termino Comunicaciones IP-40
    exit.


CSDV-IMPRIMIR-SIGUIENTE.
;---------------------
if p-15-documento =  nNroDoc
   build CAB-CSDV-003    print
   seek NEXT in PRIN-15-F
     when EOF exit.
   read PRIN-15-F
   loop
end-if
exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                     Devolucion de Envases de Consignaciขn              ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

Devolver-Env.
;------------
    PERFORM LeeHoraFecha                       ; Lee Hora y Fecha en Variables.

    perform OPENREF-ENV-CSG-F.
         when ER exit.
    seek first in ENV-CSG-F
      when EOF exit.
    search FIRST for nCodCli           in ENV-CSG-F
      when ER  beep
               message  1, '****************'
               message 17, ' No se le   Debe'
               message 33, ' Envases al Clte'
               message 49, '****************'
               wait 1 seconds.

      when OK  read ENV-CSG-F
               move cod-pro-env-csg to busca-pro-ws
               perform BUSCA-COD-PROD
                    when ER exit.
               perform Dev_MUEVE-FOLIO
               PERFORM ShowDocActual
                    when ER exit.
               perform Dev_Muestra
                    when ER exit.
      end-when

    exit.

Dev_MUEVE-FOLIO.
;--------------
    move '17' to codigo-tab-fol
    perform LEE-FOLIO
    exit.

Dev_Muestra.
;----------
;   move nro-doc-env-csg to cNroDoc
    move cod-pro-env-csg to cCodPrd
    move nround-mae-pro  to nNroUnd
    move descri-mae-pro  to d-producto-ws
    move can-pre-env-csg to can-producto-ws
    move can-pre-env-csg to nSaldo
    move fecha-env-csg   to fecha-emisi-ws

    move cod-pro-env-csg to busca-pro-ws
    perform Busca-Cod-Prod

    perform Muestra-Cant-Prod
    move can-producto-ws to nStock
    move can-pre-env-csg to can-producto-ws

    show ENVASES-DOS-SD
    perform Dev_Verifica_item
            when OK READ PRIN-19-F
                    move p-19-cnt-prod to  nCntPrd
                    move nSaldo            to nNumero1
                    move p-19-cnt-prod     to nNumero2
                    perform Iv_Resta_Enteros
                    move nNumero1          to nSaldo
                    show ENVASES-ADEL-SD
                    show CANT-PRODUCTO-SD.
            when ER show ENVASES-ADEL-SD
                    perform Dev_MUESTRA-DOCUMENTO.

    perform lee-tecla-action
        when ENTER if can-producto-ws > 0
                      perform Dev_Borra_Item
                      perform Dev_Acepta_Cantidad
                              when ER loop.
                      perform Dev_Confirma_Item      ; Confirma Item ?
                              when ER loop.
                      perform Dev_Graba_Spool
                      perform Dev_AVANZA-PRODUCTO
                   else
                      beep
                   end-if
        end-when
        when DAR   perform Dev_AVANZA-PRODUCTO.
        when UAR   perform Dev_ANTERIOR-PRODUCTO.
        when FAR   perform Dev_MUESTRA-FEC-DOC.
        when KEND  perform Dev_Ver_Doc
                       when ER exit.
                   perform Dev_CONFI-DV
                       when ER perform Dev_Borra_Todo exit.
                   perform Dev_IMPRIMIR
                           when OK perform Dev_Graba
                                   perform Dev_ACTUALIZA-FOLIO.
                           when ER perform Dev_Borra_Todo.
                   exit.
        when KELIM perform Dev_Borra_Item.

      loop.

Dev_Imprimir.
;------------
move OK to last-key
exit.

Dev_Borra_Item.
;-------------
perform Dev_Verifica_item
   when OK DELETE RECORD FROM PRIN-19-F.
exit.

Dev_Ver_Doc.
;----------
perform OPENUP-PRIN-19-F
search FIRST for nNroDoc in PRIN-19-F OFFSET IS p-19-documento
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.


Dev_Verifica_Item.
;----------------
perform OPENUP-PRIN-19-F
move cNroDoc       to Documento
move cCodPrd       to Producto
search first for Clave_Envases in PRIN-19-F
       when EOF move ER  to last-key.
exit.


Dev_BUSCA-DEVOLUCION.
;-------------------
search FIRST for nNroDoc in ENV-CSG-F
   when EOF move ER to last-key exit.
   when ER exit.
   when OK exit.
exit.


Dev_MUESTRA-DOCUMENTO.
;--------------------
if can-producto-ws <= 0
   message 49, '  Ya devueltos  '
   beep
   wait 1 second
end-if
show Dv-SHOW-DOC
exit.

Dev_MUESTRA-FEC-DOC.
;------------------
show ENVASES-UNO-SD
action
     when enter move OK to last-key
                exit.
     when KEND exit.
loop.

ADE-CSG-ANTERIOR.
;-----------------
   seek prior in ENV-CSG-F
   when ok read ENV-CSG-F.
EXIT.


Dev-primer-producto.
;-----------------
   search first for nCodCli           in ENV-csG-F
   when er move eof to last-key.
   when ok read ENV-CSG-F
           move cod-pro-env-csg to busca-pro-ws
           perform BUSCA-COD-PROD
                   when ER exit.
   end-when
exit.

Dev_AVANZA-PRODUCTO.
;------------------
search next for nCodCli           in ENV-CSG-F
when er message 49, '-Fin de archivo-'
        beep
        wait 1 second
        perform Dev_ultimo_producto.

when ok read ENV-CSG-F
        move cod-pro-env-csg to busca-pro-ws
        perform BUSCA-COD-PROD
                when ER exit.
        end-when
exit.

Dev_ultimo_producto.
;------------------
search first for Clave_Envases in ENV-csg-F
       when EOF move ER  to last-key.
exit.



Dev_ANTERIOR-PRODUCTO.
;---------------------
   perform ade-csg-anterior until nCodCli = codigo-env-csg or last-key = EOF
   if last-key = OK
      move cod-pro-env-csg to busca-pro-ws
      perform BUSCA-COD-PROD
              when ER exit.
   else
      message 49, '-Ini de archivo-'
      beep
      wait 1 second
      perform Dev-primer-producto
      move eof to last-key
   end-if
exit.

Dev_Acepta_Cantidad.
;------------------
    move  can-producto-ws to nCntPrd
    if nCntPrd <= 0
       exit.
    move 0  to nCntPrd
    accept CANT-PRODUCTO-SD
        when OK    if nCntPrd = 0 loop.
                   if nCntPrd > nStock
                      beep
                      message 49, ' No Existe Stock '
                      wait 1 seconds
                      move ER to last-key
                      exit
                   end-if
                   move nCntPrd to nCantidad
                   perform Iv_Normal_Entero
                   move nCantidad to nCntPrd

                   if nCntPrd > can-producto-ws
                      beep
                      message 49, ' Cantidad Mayor '
                      wait 1 seconds
                      move ER to last-key
                   end-if
                   exit
        end-when
        when KEND  move ER TO last-key  exit.
    loop.


Dev_CONFI-DV.
;-------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '    Devolucion  '.
    message 49, '    Yes / No    '.
    action
             when ENTER move 1 to nAtencion
                        move OK to last-key exit.

             when KEND  move ER to last-key exit.
             when NO    move ER to last-key exit.
    loop.

Dev_ACTUALIZA-FOLIO.
;--------------
    move '17' to ws-cod-folio              ; 03 para DEVOLUCIONES.
    search FIRST for ws-cod-folio in TAB-FOL-F
    add 1  to folio-tab-fol
    rewrite REG-TAB-FOL
    exit.

Dev_Confirma_Item.
;----------------
    message  1, '****************'.
    message 17, 'Confirma Ingreso'.
    message 33, '    Yes / No    '.
    message 49, '****************'.
    perform lee-tecla-action
        when ENTER exit.
        when NO    move ER to last-key exit.
    loop.


Dev_Graba_Spool.
;--------------
perform OPENUP-PRIN-19-F

move  nNroDoc           to   p-19-documento      ; Documento de la Devoluciขn.
;move  nro-doc-env-csg   to   p-19-doc-pre        ; Documento de Referencia.
move  nCodCli           to   p-19-cod-cli        ; Cขdigo de Cliente
move  cod-pro-env-csg   to   p-19-cod-prod       ; Cขdigo de Producto
move  nCntPrd           to   p-19-cnt-prod       ; Cantidad de Producto.

move  cNomcli           to   p-19-nom-cli
move  cDirCli           to   p-19-dir-cli
move  dia-ws            to   p-19-dia
move  mes-ws            to   p-19-mes
move  ano-ws            to   p-19-ano
move  hrs-ws            to   p-19-hora
move  minu-ws           to   p-19-minu
move  sec-ws            to   p-19-segu
move  descri-mae-pro    to   p-19-dsc-prod
move  nNroUnd           to   p-19-und-prod
write REG-PRINT-19
exit.


Dev_Graba.
;--------
perform OPENREF-PRIN-19-F
search first for nNroDoc in PRIN-19-F OFFSET IS p-19-documento
       when EOF exit.
       when ER exit.
       when OK read PRIN-19-F.

perform OPENUP-DevCsg-F
move p-19-documento  TO nro-doc-dev-csg       ;
move p-19-doc-pre    TO doc-ref-dev-csg           ; De Prstamo.
move p-19-cod-cli    TO cod-cli-dev-csg
move p-19-cod-prod   TO cod-pro-dev-csg
move p-19-cnt-prod   TO can-pre-dev-csg
move nFecha          to fecha-dev-csg
move nCodVd          to vend-dev-csg
move nRuta           to ruta-dev-csg
move x'0D'           TO cr-dev-csg
move x'0A'           TO lf-dev-csg
write REG-DevCsg


perform OPENUP-ENV-csg-F
move nCodCli       to Cliente
move p-19-cod-prod to ProductoCsg
search FIRST for Clave_consignacion in ENV-csg-F
       when ER  message 1, "Det_csd".
       when EOF message 1, "Det_env".
       when OK  read ENV-CSG-F
                move p-19-und-prod     to nNroUnd
                move can-pre-env-csg   to nNumero1
                move p-19-cnt-prod     to nNumero2
                perform Iv_Resta_Enteros
                move nNumero1          to can-pre-env-csg
                if nNumero1 = 0
                   DELETE RECORD FROM ENV-CSG-F
                else
                   rewrite  REG-ENV-CSG
                end-if
       end-when

move p-19-cod-prod to  cCodPrd
move p-19-cnt-prod to  nCntPrd
move p-19-und-prod to  nNroUnd
perform Iv_Sub_Producto
;perform Iv_Add_Devolucion
perform Dev_Borra
loop.

;Iv_Add_Devolucion.
;;-----------------
;perform OPENREF-ARC-INV
;search FIRST for cCodPrd in ARC-INV-F
;       when OK  read ARC-INV-F
;                move den-arc-inv     to nNumero1
;                move nCntPrd         to nNumero2
;                perform Iv_Suma_Enteros
;                move nNumero1        to den-arc-inv
;                rewrite REG-ARC-INV.
;exit.


Dev_Borra.
;--------
perform OPENUP-PRIN-19-F
search first for nNroDoc in PRIN-19-F OFFSET IS p-19-documento
       when OK DELETE RECORD FROM PRIN-19-F.
exit.


Dev_Borra_todo.
;-------------
perform Dev_Borra
       when OK loop.
exit.



;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                 VARIOS DURANTE LA RUTA                  ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
VARIOS-VENTA.
;------------
    move '    Varios     ' to proc-ws.
    perform VARIOS-000.
         when ER exit.
    move 1 to punt-varios.
    perform VARIOS-001.
         when ER exit.
    exit.

VARIOS-000.
;----------
    move 0   to sw-error.
    move ' ' to cNroDoc.
    perform MOVE-FOLIO.
         when ER exit.
    move 1 to punt-ingreso.
    perform ACCEP-DATOS-VARIOS
         when ER exit.
    perform CONFIRMA-VARIOS
         when ER exit.
    if sw-error = 1 loop.
    exit.

MOVE-FOLIO.
;----------
    move '05' to codigo-tab-fol
    perform lee-folio
    move folio-tab-fol to cNroDoc.
    exit.

ACCEP-DATOS-VARIOS.
;------------------
    examine punt-ingreso
         when 0 move 1 to punt-ingreso loop.
         when 1 show NUM-DOCUM-SD
;               wait 1 second
                move OK to last-key
           end-when
         when 2 move 'N' to shift-char
                move ' ' to nombre-estable-ws
                accept ESTABLE-NOM-SD
                    when OK move OK to last-key.
           end-when
         when 3 move 'N' to shift-char
                move ' ' to motivo-trans-ws
                accept MOTIVO-TRANS-SD
                    when OK move OK to last-key.
           end-when
         when 4 move '@' to shift-char, exit.
    no-op.
         when OK   add 1 to punt-ingreso.
         when FAR  subtract 1 from punt-ingreso.
         when KEND move ER to last-key exit.
    loop.

CONFIRMA-VARIOS.
;---------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, 'Ingreso de Datos'.
    message 49, '    Yes / No    '.
    perform lee-tecla-action
             when ENTER perform OPENUP-CAB-VAR-F
                        move cNroDoc      to nro-doc-cab-var
                        move nombre-estable-ws to nombre-cab-var
                        move motivo-trans-ws   to motivo-cab-var
                        search first for nro-doc-cab-var in CAB-VAR-F
                        when ok rewrite REG-CAB-VAR.
                        when er move 0         to dinero-cab-var
                                write   REG-CAB-VAR
                                perform UPDATE-FOLIO
                                     when ER exit.
                        end-when
                        exit.
             when NO    move 1 to sw-error, exit.
    loop.

UPDATE-FOLIO.
;------------
    move '05' to codigo-tab-fol
    perform lee-folio

    add 1  to folio-tab-fol.
    rewrite REG-TAB-FOL.
    exit.

VARIOS-001.
;----------
    message 1,  '****************'.
    examine punt-varios
            when 0 move 2 to punt-varios    loop.
            when 1 message 17 ' Recoge Envases '.
            when 2 message 17 ' Recoge  Dinero '.
            when 3 move 1 to punt-varios    loop.
    message 49, '****************'.
    message 33, '    Yes / No    '.
    perform lee-tecla-fast-action
            when ENTER perform SUB-MENU-VARIOS.
;                      exit. ;asp 21/10/93
            when KEND  perform impr-compr-varios
                       exit.
            when NO    add 1 to punt-varios.
            when FAR   add 1 to punt-varios.
            when BAR   subtract 1 from punt-varios.
    loop.

IMPR-COMPR-VARIOS.
;----------------
    perform SETEA-IMPRESORA
            when ER exit.
            when KEND move OK to last-key exit.
    clear.
    message 1, '* IMPRIMIENDO *'.

    build CAB-FACT-000   print.
    build CABECERA-RUC   print.
    build COMPR-VARIOS-00 print ;titulo
    build COMPR-VARIOS-01 print ;documento
    build COMPR-VARIOS-02 print ;cliente
    build COMPR-VARIOS-03 print ;motivo
    build COMPR-VARIOS-04 print ;dinero
    perform OPENUP-DET-VAR-F
    search FIRST for nro-doc-cab-var in DET-VAR-F
    when OK read DET-VAR-F
            build RAYA    print ;raya
            lprint '       Descripcion         Cantidad'
            perform COMPR-VARIOS-LOOP.
    build FIRMA-VENDEDOR print
    build FIN-LISTADO    print
    disconnect
exit.

COMPR-VARIOS-LOOP.
;-----------------
   search first for cod-pro-det-var in MAE-PRO-F
           when ok read MAE-PRO-F
           build COMPR-VARIOS-05 print. ;detalle de productos
   search next for nro-doc-cab-var in DET-VAR-F
          when er exit.
   read DET-VAR-F
loop.

SUB-MENU-VARIOS.
;---------------
    examine punt-varios
            when 1 perform RECOGE-ENVASES.
            when 2 perform RECOGE-DINERO.
    exit.

RECOGE-ENVASES.
;--------------
    move 0 to cCodPrd.
    move 0 to d-producto-ws.
    move 0 to p-producto-ws.
    perform OPENREF-MAE-PRO-F.
         when ER exit.
    seek first in MAE-PRO-F.
      when EOF exit.
    read MAE-PRO-F.
    perform RECOGE-ENV-001.
         when ER exit.
    exit.

RECOGE-ENV-001.
;--------------
    move codigo-mae-pro to cCodPrd.
    move descri-mae-pro to d-producto-ws.
    move precio-mae-pro to p-producto-ws.
    move nround-mae-pro to  nNroUnd

    show P-PRODUCTOS-SD.
    perform lee-tecla-fast-action
             when ENTER perform RECOGE-ENV-002
                             when ER exit.
                        perform RECOGE-ENV-003
                             when ER exit.
                        perform AVA-RECOGE-ENV
               end-when
             when DAR   perform AVA-RECOGE-ENV.
             when UAR   perform RET-RECOGE-ENV.
             when KEND  exit.
    loop.

AVA-RECOGE-ENV.
;--------------
    seek NEXT in MAE-PRO-F
         when EOF message 49, '-Final  archivo-'
                  beep
                  wait 1 second
                  seek last in MAE-PRO-F.
         when OK  read MAE-PRO-F.
    exit.

RET-RECOGE-ENV.
;--------------
    seek PRIOR in MAE-PRO-F
         when EOF message 49, '-Inicio archivo-'
                  beep
                  wait 1 second
                  seek first in MAE-PRO-F.
         when OK  read MAE-PRO-F.
    exit.

RECOGE-ENV-002.
;--------------
    move 0 to nCntPrd.
    accept CANT-PROD-SD
        when OK  move nCntPrd to nCantidad
                 perform Iv_Normal_Entero
                 move nCantidad to nCntPrd
                 exit.
        when KEND exit.
    loop.

RECOGE-ENV-003.
;--------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '     Envase     '.
    message 49, '    Yes / No    '.
    perform lee-tecla-action
             when ENTER perform OPENUP-DET-VAR-F
                        move cNroDoc  to nro-doc-det-var
                        move cCodPrd to cod-pro-det-var
                        move nCntPrd  to canti-det-var
                        write REG-DET-VAR
                        ;asp
                        move cCodPrd to codigo-arc-inv
                        perform Iv_Add_Producto
                        ;asp
                        exit.
               end-when
             when NO    exit.
    loop.


RECOGE-DINERO.
;-------------
    perform ac-rec-dinero
            when kend exit.
    add  caja-ini-ws to dinero-cab-var
    rewrite REG-CAB-VAR                ;siempre apunta al registro correcto.

    perform Caj_Ini
    add caja-ini-ws  to caja-arc-car
    rewrite REG-ARC-CAR
    exit.

;RECOGE-DINERO. ;No usado !!
;-------------
;   perform ac-rec-dinero
;   when kend exit.
;   perform OPENUP-CAB-VAR-F
;   move cNroDoc to nro-doc-cab-var
;   search first for nro-doc-cab-var in CAB-VAR-F
;   when ok read CAB-VAR-F
;           add  caja-ini-ws to dinero-cab-var
;           rewrite REG-CAB-VAR.
;   when er fill REG-CAB-VAR with ' '
;           move cNroDoc to nro-doc-cab-var
;           move caja-ini-ws  to dinero-cab-var
;           write REG-CAB-VAR.
;   exit.

AC-REC-DINERO.
;-------------
   accept REC-DINERO-SD
   when ok if caja-ini-ws = 0 loop.
           exit.
   when kend exit.
loop.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                          CENSO                          ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
CENSO-VENTA.
;-----------
    PERFORM LeeHoraFecha
    examine sw-censo
            when 0 perform ERROR-CENSO.
      ;     when 1 perform CENSO-PRECIOS.
      ;     when 2 perform CENSO-ENVASES.
      ;     when 3 perform MERCHANDISING.
      ;     when 4 perform PUBLICIDAD.
    clear
    exit.

ERROR-CENSO.
;-----------
    message 1,  '****************'
    message 17, '  Para Hoy, No  '
    message 33, '   Hay Censo    '
    message 49, '****************'
    beep
    wait 2 tenths
    beep
    wait 2 tenths
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                                   CENSO                                    ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
;CENSO-ENVASES.
;;-------------
;    move ' Censo Envases  ' to proc-ws.
;    perform OPENREF-PROD-CENSO
;          when ER exit.
;    seek FIRST in PRODUC-CENSO-F
;            when EOF message  1, '****************'
;                     message 17, ' Archivo  Censo '
;                     message 33, '     Vacio      '
;                     message 49, '****************'
;                     wait 1 second
;                     exit.
;    read PRODUC-CENSO-F.
;
;    perform OPENREF-EMBOTELLADORA
;          when ER exit.
;    seek FIRST in EMBOTELLADORA-F
;            when EOF message  1, '****************'
;                     message 17, '    Archivo     '
;                     message 33, ' Embotelladoras '
;                     message 49, '     Vacio      '
;                     wait 1 second
;                     exit.
;    read EMBOTELLADORA-F.
;
;    perform OPENUP-C-ENVASE.
;
;CENSO-002.
;;---------
;    move codigo-mae-cli to busca-censo1.
;    move prce-codigo    to busca-censo2.
;    move 0              to cant-censo.
;
;    show ENVASE-CENSO-SD.
;    accept CANTI-CENSO-SD
;         when KEND  exit.
;         when OK    if cant-censo = 0 loop.
;                    search FIRST for busca-censo in CENSO-F
;                           when OK read CENSO-F
;                                   add cant-censo to cens-cantidad
;                                   move x'0D'     to cens-c1
;                                   move x'0A'     to cens-c2
;                                   rewrite REG-CENSO.
;                           when ER move codigo-mae-cli  to cens-cliente
;                                   move prce-codigo     to cens-producto
;                                   move cant-censo      to cens-cantidad
;                                   move x'0D'           to cens-c1
;                                   move x'0A'           to cens-c2
;                                   write REG-CENSO.
;                    loop.
;         when KINS  search FIRST for busca-censo in CENSO-F
;                           when OK read CENSO-F
;                                   move cens-cantidad to cant-censo.
;                    beep
;                    show CANTI-CENSO-SD
;                    wait 1 second
;                    loop.
;         when KMOD  perform MODIFICA-CANT-CENSO
;                    loop.
;         when KNEXT perform SGTE-PROD-EMP
;                        when EOF exit.
;                    seek NEXT in EMBOTELLADORA-F
;                        when EOF exit.
;                    read EMBOTELLADORA-F
;                    loop.
;         when FAR   seek NEXT in PRODUC-CENSO-F
;                         when EOF message 49, '- Fin  Empresa -'
;                                  beep
;                                  wait 1 second
;                                  seek LAST in PRODUC-CENSO-F.
;                    read PRODUC-CENSO-F
;                    if embo-codigo not = prce-cod1
;                       message 49, '- Fin  Empresa -'
;                       beep
;                       wait 1 second
;                       seek PRIOR in PRODUC-CENSO-F
;                       read PRODUC-CENSO-F.
;                    loop.
;         when BAR   seek PRIOR in PRODUC-CENSO-F
;                         when EOF message 49, '-Inicio Empresa-'
;                                  beep
;                                  wait 1 second
;                                  seek FIRST in PRODUC-CENSO-F.
;                    read PRODUC-CENSO-F
;                    if embo-codigo not = prce-cod1
;                       message 49, '-Inicio Empresa-'
;                       beep
;                       wait 1 second
;                       seek NEXT in PRODUC-CENSO-F
;                       read PRODUC-CENSO-F.
;                    loop.
;    loop.
;
;SGTE-PROD-EMP.
;;-------------
;    seek NEXT in PRODUC-CENSO-F
;        when EOF exit.
;    read PRODUC-CENSO-F.
;    if embo-codigo = prce-cod1 loop.
;    exit.
;
;MODIFICA-CANT-CENSO.
;;-------------------
;    move 0 to cant-censo.
;    search FIRST for busca-censo in CENSO-F
;           when ER exit.
;    read CENSO-F.
;    show TOT-MODI-CENSO-SD.
;    accept MODI-CENSO-SD
;          when KEND  exit.
;          when ER    loop.
;    move cant-censo to cens-cantidad.
;    move x'0D'      to cens-c1
;    move x'0A'      to cens-c2
;    rewrite REG-CENSO.
;    show TOT-MODI-CENSO-SD.
;    message 49, '                '.
;    wait 1 second.
;    exit.
;
;;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;;ณ                              ENCUESTA   PRECIO                             ณ
;;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
;CENSO-PRECIOS.
;;-------------
;    move 'Encuesta Precios' to proc-ws.
;    perform OPENREF-PROD-CENSO
;          when ER exit.
;    seek FIRST in PRODUC-CENSO-F
;            when EOF message  1, '****************'
;                     message 17, ' Archivo  Censo '
;                     message 33, '     Vacio      '
;                     message 49, '****************'
;                     wait 1 second
;                     exit.
;    read PRODUC-CENSO-F.
;
;    perform OPENREF-EMBOTELLADORA
;          when ER exit.
;    seek FIRST in EMBOTELLADORA-F
;            when EOF message  1, '****************'
;                     message 17, '    Archivo     '
;                     message 33, ' Embotelladoras '
;                     message 49, '     Vacio      '
;                     wait 1 second
;                     exit.
;    read EMBOTELLADORA-F.
;
;    perform OPENUP-C-PRECIO.
;
;ENCUESTA-002.
;;------------
;    move codigo-mae-cli to busca-censo1.
;    move prce-codigo    to busca-censo2.
;    move 0              to cant-censo.
;    show ENVASE-CENSO-SD.
;    accept CANTI-ENCUESTA-SD
;         when KEND  exit.
;         when OK    if cant-encuesta = 0 loop.
;                    search FIRST for busca-censo in ENCUESTA-F
;                           when OK read ENCUESTA-F
;                                   move cant-encuesta to encu-precio
;                                   move x'0D'         to encu-c1
;                                   move x'0A'         to encu-c2
;                                   rewrite REG-ENCUESTA.
;                           when ER move busca-censo1  to encu-codigo
;                                   move busca-censo2  to encu-producto
;                                   move cant-encuesta to encu-precio
;                                   move x'0D'         to encu-c1
;                                   move x'0A'         to encu-c2
;                                   write REG-ENCUESTA.
;                    perform ENCUESTA-PROXIMA
;                    loop.
;         when KINS  search FIRST for busca-censo in ENCUESTA-F
;                           when OK read ENCUESTA-F
;                                   move encu-precio to cant-encuesta.
;                    beep
;                    show CANTI-ENCUESTA-SD
;                    wait 1 second
;                    loop.
;         when KNEXT perform SGTE-PROD-EMP
;                        when EOF exit.
;                    seek NEXT in EMBOTELLADORA-F
;                        when EOF exit.
;                    read EMBOTELLADORA-F
;                    loop.
;         when FAR   perform ENCUESTA-PROXIMA
;                    loop.
;         when BAR   seek PRIOR in PRODUC-CENSO-F
;                         when EOF message 49, '-Inicio Empresa-'
;                                  beep
;                                  wait 1 second
;                                  seek first in PRODUC-CENSO-F.
;                    read PRODUC-CENSO-F
;                    if embo-codigo not = prce-cod1
;                       message 49, '-Inicio Empresa-'
;                       beep
;                       wait 1 second
;                       seek next in PRODUC-CENSO-F
;                       read PRODUC-CENSO-F.
;                    loop.
;    loop.
;
;ENCUESTA-PROXIMA.
;;----------------
;    seek NEXT in PRODUC-CENSO-F
;         when EOF message 49, '- Fin  Empresa -'
;                  beep
;                  wait 1 second
;                  seek LAST in PRODUC-CENSO-F.
;    read PRODUC-CENSO-F.
;    if embo-codigo not = prce-cod1
;       message 49, '- Fin  Empresa -'
;       beep
;       wait 1 second
;       seek PRIOR in PRODUC-CENSO-F
;       read PRODUC-CENSO-F.
;    exit.
;
;MERCHANDISING.
;;-------------
;    move 'Encta. Merchand.' to proc-ws.
;    move 'C' to shift-char
;    perform ac-merchand
;    move '@' to shift-char
;    when kend exit.
;    move codigo-mae-cli to CM-COD-CLI
;    move x'0D' to cm-c1
;    move x'0A' to cm-c2
;    perform openup-merchand
;    search first for CM-COD-CLI in MERCHAND-F
;    when ok rewrite REG-MERCHAND.
;            exit.
;    when er write REG-MERCHAND.
;    exit.
;
;AC-MERCHAND.
;;----------
;   accept txt-merchand-sd
;   when kend exit.
;   when ok exit.
;loop.
;
;PUBLICIDAD.
;;----------
;    move 'Encta. Publicid.' to proc-ws.
;    perform OPENREF-ITEM-PUBLI
;          when ER exit.
;    seek FIRST in ITEM-PUBLICID-F
;            when EOF message  1, '****************'
;                     message 17, ' Archivo Public.'
;                     message 33, '     Vacio      '
;                     message 49, '****************'
;                     wait 1 second
;                     exit.
;    read ITEM-PUBLICID-F
;
;    perform OPENUP-PUBLICIDAD
;
;PUBLICID-002.
;;------------
;    move CODIGO-MAE-CLI to CP-COD-CLI
;    move IP-COD-ITEM    to CP-COD-ITEM
;    move x'0D'          to cp-c1
;    move x'0A'          to cp-c2
;    show publicid-sd
;    accept CANT-PUBLICIDAD-SD
;         when KEND  exit.
;         when OK    if cant-encuesta = 0 loop.
;                    search FIRST for CLAVE-PUBLICIDAD in PUBLICIDAD-F
;                           when OK rewrite REG-PUBLICIDAD.
;                           when ER write REG-PUBLICIDAD.
;                    perform PUBLICID-PROXIMA
;                    loop.
;         when KINS  move 0 to CP-CANTIDAD
;                    search FIRST for CLAVE-PUBLICIDAD in PUBLICIDAD-F
;                           when OK read PUBLICIDAD-F.
;                    beep
;                    show CANT-PUBLICIDAD-SD
;                    wait 1 second
;                    loop.
;         when FAR   perform PUBLICID-PROXIMA.
;         when BAR   perform PUBLICID-PREVIA.
;    loop.
;
;PUBLICID-PROXIMA.
;;----------------
;    seek NEXT in ITEM-PUBLICID-F
;         when EOF message 49, '- Fin Publicid -'
;                  beep
;                  wait 1 second
;                  seek LAST in ITEM-PUBLICID-F.
;    read ITEM-PUBLICID-F
;    exit.
;
;PUBLICID-PREVIA.
;;----------------
;    seek PRIOR in ITEM-PUBLICID-F
;         when EOF message 49, '- Ini Publicid -'
;                  beep
;                  wait 1 second
;                  seek FIRST in ITEM-PUBLICID-F.
;    read ITEM-PUBLICID-F
;    exit.
;
;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                              MENU SUPERVISOR                               ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
;MENU-SUPERVISOR.
;;---------------
;    move ' Menu Principal' to proc-ws.
;    message 1,  '****************'.
;    examine punt-mp
;            when 0 move 3 TO punt-mp        loop.
;            when 1 message 17, '    MUESTREO    '.
;            when 2 message 17, '    INFORME     '.
;            when 3 message 17, ' ACTIVAR CENSO  '.
;            when 4 move 1 TO punt-mp        loop.
;    message 49, '****************'.
;    perform lee-tecla-fast-action
;            when ENTER perform PROCESO-MAIN.
;            when FAR   add 1 to punt-mp.
;            when BAR   subtract 1 from punt-mp.
;            when KEND  perform SELECCIONA-MENU exit.
;    loop.
;
;PROCESO-MAIN.
;;------------
;    PERFORM LeeHoraFecha
;    examine punt-mp
;            when 1 perform MUESTREO.
;            when 2 perform INFORME.
;            when 3 perform ACTIVAR-CENSO.
;    clear
;    exit.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                 MUESTREO DE SUPERVISION                 ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
;MUESTREO.
;;--------
;    move 0 to cant-muestreo-ws.
;    accept CANT-MUESTREO-SD.
;        when OK   if cant-muestreo-ws = 0 loop.
;                  next.
;        when KEND exit.
;    loop.
;
;MUESTREO-000.
;;------------
;    move 1 to fin-ws.
;    perform OPENUP-PRIN-06-F.
;    erase PRIN-06-F.
;    perform OPENUP-PRIN-06-F.
;    perform OPENREF-ARC-ATE-F
;         when ER  exit.
;    seek FIRST in ARC-ATE-F
;         when EOF exit.
;    perform MUESTREO-001 until fin-ws >= cant-muestreo-ws
;         when ER exit.
;    perform REPORTE-MUESTREO
;         when ER exit.
;    exit.
;
;MUESTREO-001.
;;------------
;    read ARC-ATE-F.
;    move codigo-arc-ate  to p-06-estado.
;   ;move motivo-arc-ate  to p-06-estado.
;    move p-06-codigo     to b1-fact.
;    move p-06-factura    to b2-fact.
;    perform OPENREF-CFA-VTA-F
;         when ER move cant-muestreo-ws to fin-ws
;                 exit.
;    search FIRST for busca-factura in CFA-VTA-F
;         when ER perform MUESTREO-002
;                      when ER move cant-muestreo-ws to fin-ws
;                              exit.
;                 loop.
;    read CFA-VTA-F.
;    move hora-cfa-vta   to p-06-hora.
;    move minu-cfa-vta   to p-06-minu.
;    move segu-cfa-vta   to p-06-segu.
;   ;move n-estb-cfa-vta to p-06-nomb-esta.
;   ;move direcc-cfa-vta to p-06-direccion.
;    write REG-PRINT-06.
;    seek NEXT in ARC-ATE-F
;      when EOF move cant-muestreo-ws to fin-ws
;               exit.
;    add 1 to fin-ws
;    loop.
;
;MUESTREO-002.
;;------------
;    perform OPENREF-MAE-CLIE.
;         when ER exit.
;    search FIRST for p-06-codigo in MAE-CLI-F
;        when ER exit.
;    read MAE-CLI-F.
;    move 0              to p-06-hora.
;    move 0              to p-06-minu.
;    move 0              to p-06-segu.
;    move n-estb-mae-cli to p-06-nomb-esta.
;    move direcc-mae-cli to p-06-direccion.
;    write REG-PRINT-06.
;    seek NEXT in ARC-ATE-F
;      when EOF exit.
;    add 1 to fin-ws
;    exit.
;
;;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;;ณ                    REPORTE MUESTREO                     ณ
;;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
;REPORTE-MUESTREO.
;;----------------
;    perform SETEA-IMPRESORA
;            when ER exit.
;            when KEND move OK to last-key exit.
;    clear.
;    message 1, '* IMPRIMIENDO *'.
;    perform OPENREF-PRIN-06-F
;         when ER exit.
;    next.
;
;IMP-MUES-001.
;    build CABECERA-01.   print.                 ; Imprime CABECERAS
;    build CABECERA-02.   print.
;    build CAB-MUES-001.  print.
;    next.
;
;IMP-MUES-002.
;    read PRIN-06-F.
;    build DET-MUES-001.  print.
;    seek NEXT in PRIN-06-F.
;      when EOF next.
;    loop.
;
;IMP-MUES-003.
;    build FIN-LISTADO.   print.
;    disconnect.                                 ; Termino Comunicaciones IP-40
;    exit.
;
;;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;;ณ                 INFORME DE SUPERVISION                  ณ
;;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
;INFORME.
;;-------
;    message  1, '****************'.
;    message 17, 'Imprime  Informe'.
;    message 33, '    Yes / No    '.
;    message 49, '****************'.
;    perform lee-tecla-fast-action
;             when ENTER move 0 to tot1-ws
;                        move 0 to tot2-ws
;                        move 0 to tot3-ws
;                        move 0 to tot4-ws
;                        perform INFORME-000
;                             when ER exit.
;                        perform INFORME-005
;                             when ER exit.
;                        perform REPORTE-INFORME
;                             when ER exit.
;                        exit.
;             when KEND  exit.
;             when NO    exit.
;    loop.
;
;INFORME-000.
;;-----------
;    perform OPENREF-MAE-CLIE
;         when ER  exit.
;    seek FIRST in MAE-CLI-F
;         when EOF exit.
;    perform INFORME-001
;         when ER exit.
;    perform OPENREF-ARC-ATE-F
;         when ER  exit.
;    seek FIRST in ARC-ATE-F
;         when EOF exit.
;    perform INFORME-002
;         when ER exit.
;    perform OPENREF-CFA-VTA-F
;         when ER exit.
;    seek FIRST in CFA-VTA-F
;         when EOF exit.
;    perform INFORME-003
;         when ER exit.
;    perform OPENREF-ARC-COB-F
;         when ER exit.
;    seek FIRST in ARC-COB-F
;         when EOF exit.
;    perform INFORME-004
;         when ER exit.
;    exit.
;
;INFORME-001.
;;-----------
;    read MAE-CLI-F.
;    add 1 to tot1-ws.
;    seek NEXT in MAE-CLI-F
;      when EOF exit.
;    loop.
;
;INFORME-002.
;;-----------
;    read ARC-ATE-F.
;    add 1 to tot2-ws.
;    seek NEXT in ARC-ATE-F
;      when EOF exit.
;    loop.
;
;INFORME-003.
;;-----------
;    read CFA-VTA-F.
;    add 1 to tot3-ws.
;    seek NEXT in CFA-VTA-F
;      when EOF exit.
;    loop.
;
;INFORME-004.
;;-----------
;    read ARC-COB-F.
;    add 1 to tot4-ws.
;    seek NEXT in ARC-COB-F
;      when EOF exit.
;    loop.
;
;INFORME-005.
;;-----------
;    perform OPENUP-PRIN-07-F.
;    erase PRIN-07-F.
;    perform OPENUP-PRIN-07-F.
;    perform OPENREF-ARC-INV
;         when ER exit.
;    seek FIRST in ARC-INV-F
;         when EOF exit.
;    perform INFORME-006
;         when ER exit.
;    exit.
;
;INFORME-006.
;;-----------
;    read ARC-INV-F.
;    move codigo-arc-inv to busca-pro-ws.
;    move codigo-arc-inv to p-07-codigo.
;    move ini-arc-inv    to p-07-inicial.
;    move vco-arc-inv    to p-07-ventas.
;    move canti-arc-inv  to p-07-final.
;    perform BUSCA-COD-PROD
;         when ER exit.
;    move descri-mae-pro to p-07-descri.
;    write REG-PRINT-07.
;    seek NEXT in ARC-INV-F
;      when EOF exit.
;    loop.
;
;;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;;ณ                     REPORTE INFORME                     ณ
;;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
;REPORTE-INFORME.
;;---------------
;    perform SETEA-IMPRESORA
;            when ER exit.
;            when KEND move OK to last-key exit.
;    clear.
;    message 1, '* IMPRIMIENDO *'.
;    perform OPENREF-PRIN-07-F
;         when ER exit.
;    next.
;
;IMP-INFO-001.
;    build CABECERA-01.   print.                 ; Imprime CABECERAS
;    build CABECERA-02.   print.
;    build CAB-INFO-001.  print.
;    next.
;
;IMP-INFO-002.
;    read PRIN-07-F.
;    build DET-INFO-001.  print.
;    seek NEXT in PRIN-07-F.
;      when EOF next.
;    loop.
;
;IMP-INFO-003.
;    build DET-INFO-002.  print.
;    build PIE-PAGINA-01. print.                 ; Imprime Pie de Pagina
;    build PIE-PAGINA-02. print.
;    build FIN-LISTADO.   print.
;    disconnect.                                 ; Termino Comunicaciones IP-40
;    exit.
;
;;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;;ณ                   ACTIVADOR DE CENSO                    ณ
;;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
;ACTIVAR-CENSO.
;;-------------
;move 0 to sw-censo.
;move 1 to punt-censo.
;
;CENSO-000.
;;---------
;    move 'Activando Censo' to proc-ws.
;    message 1,  '****************'.
;    examine punt-censo
;            when 0 move 5 TO punt-censo     loop.
;            when 1 message 17, ' Censo Precios  '.
;            when 2 message 17, ' Censo Envases  '.
;            when 3 message 17, ' Merchandising  '.
;            when 4 message 17, '   Publicidad   '.
;            when 5 message 17, 'Desactiva  Censo'.
;            when 6 move 1 TO punt-censo     loop.
;    message 49, '****************'.
;    perform lee-tecla-fast-action
;            when ENTER perform CENSO-001.
;            when FAR   add 1 to punt-censo.
;            when BAR   subtract 1 from punt-censo.
;            when KEND  exit.
;    loop.
;
;CENSO-001.
;;---------
;    PERFORM LeeHoraFecha
;    examine punt-censo
;            when 1 move 1 to sw-censo.
;            when 2 move 2 to sw-censo.
;            when 3 move 3 to sw-censo.
;            when 4 move 4 to sw-censo.
;            when 5 move 0 to sw-censo.
;    clear
;    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                        RUTINA PARA BUSCAR PRODUCTO                         ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
BUSCA-COD-PROD.
;--------------
    perform OPENREF-MAE-PRO-F
         when ER exit.
    search FIRST for busca-pro-ws in MAE-PRO-F
         when ER message 1,  '****************'
                 message 17, '   Codigo No    '
                 message 33, '     Existe     '
                 message 49, '****************'
                 beep
                 wait 2 tenths
                 beep
                 wait 2 tenths
                 exit.
    read MAE-PRO-F
    move nround-mae-pro to  nNroUnd      ; Para la pantalla
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                               MENU OPERADOR                                ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

MENU-OPERADOR.
;-------------
move 1 to  punt-mp

MENU-OPER.
;---------
    move ' Menu Principal' to proc-ws.
    message 1,  '****************'.
    examine punt-mp
            when 0 move 1 TO punt-mp        loop.
            when 1 message 17, ' COMUNICACIONES '.
            when 2 move 1 TO punt-mp        loop.
    message 49, '****************'.
    perform lee-tecla-fast-action
            when ENTER perform MENU-SELECCION.
            when FAR   add 1 to punt-mp.
            when BAR   subtract 1 from punt-mp.
            when KEND  perform SELECCIONA-MENU exit.
    loop.

MENU-SELECCION.
;--------------
    PERFORM LeeHoraFecha
    examine punt-mp
            when 1 perform COMUNICACIONES.
    clear
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                               COMUNICACIONES                               ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
COMUNICACIONES.
;--------------
    move ' Comunicaciones ' to proc-ws.
    move 1                  to punt-comu.
  ; perform OPENREF-IDENTIF-F
  ;      when OK seek FIRST in IDENTIFICA-F
  ;                   when OK read IDENTIFICA-F.
  ;           end-when

PROCESO-COMU-001.
;----------------
    examine punt-comu
         when 1 move 0 to punt-comu.
         when 2 move 1 to punt-comu.

    perform PROCESO-COMU-010
    exit
    perform PROCESO-COMU-002.
    exit.

PROCESO-COMU-002.
;----------------
    message  1, '****************'.
    message 17, ' Coneccion O.K. '.
    message 33, '    Yes / No    '.
    message 49, '****************'.
    perform lee-tecla-action
         when ENTER perform PROCESO-COMU-010 exit.
         when NO    exit.
         when KEND  exit.
    loop.

PROCESO-COMU-010.
;----------------
    perform FILELIST-COMUNICA
            when ER exit.
    move OK to LAST-KEY
    perform LLAMA-RUTINA
    perform ini-ram-ptc
    move OK to LAST-KEY
    exit.

FILELIST-COMUNICA.
;-----------------
    clear.
    message 1 'Espere .........'.
    move 'FILELIST'  to file-name.
    open update FILELIST-F
         when ER create FILELIST-F
                 loop.
    reset FILELIST-F.
    move 'PC '             to TIPO-HOST-1
    move '96'              to VELOCIDAD-1
    move nCodVd            to CODI-VEND-1
    move 0                 to NUM-TELEFONO
    move 0                 to NUM-AREA
    move '        '        to SUBMIT-FILE
    move nCodVd            to SUBMIT-FILE
    move '        '        to IDENTIF-FILE
    move nCodVd            to IDENTIF-FILE
;   move PUNT-COMU         to COMM-MODE
    move 0                 to COMM-MODE
    write HOST-PARAMS-R
    exit.

LLAMA-RUTINA.
;------------
    move 'SUB710' to FILE-NAME
    open REF PROGRAMA-F
         when ER clear
                 beep
                 message 1, ' SUB710 NO ESTA '
                 wait 1 seconds
                 exit.
    call PROGRAMA-F
         when OK perform BORRA-DATOS
                 move 1 to carga-ok.
    move 'FILELIST' to FILE-NAME
    erase FILELIST-F
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                           CARGA  DATOS  ALL                                ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
CARGA-DATOS-ALL.
;---------------
    move ' Carga de Datos ' to PROC-WS.
    move 0                  to nCodVd
    move ' '                to shift-char.
    move 1                  to punt-comu.
    clear.
    message  1, '  Carga Datos   '.
    message 17, '****************'.
    accept CARGA-CODIGO-SD
    if nCodVd = 0 loop.
    move '@' to shift-char.


PROCESO-COM-001.
;---------------
    move 1 to punt-comu
    examine punt-comu
         when 1 move 0 to punt-comu.
         when 2 move 1 to punt-comu.
    perform PROCESO-CARGA-010
    exit.

PROCESO-CARGA-000.
;-----------------
    message  1, '****************'.
    message 17, ' Coneccion O.K. '.
    message 33, '    Yes / No    '.
    message 49, '****************'.
    perform lee-tecla-action
         when ENTER perform PROCESO-CARGA-010 exit.
         when NO    exit.
         when KEND  exit.
    loop.

PROCESO-CARGA-010.
;-----------------
    perform FILELIST-CARGA
            when ER exit.
    move OK to LAST-KEY
    perform LLAMA2-RUTINA
    perform ini-ram-ptc
    move OK to LAST-KEY
    exit.

FILELIST-CARGA.
;--------------
    clear.
    message 1 'Espere .........'.
    move 'FILELIST'  to file-name.
    open update FILELIST-F
         when ER create FILELIST-F
                 loop.
    reset FILELIST-F
    move 'PC '          to TIPO-HOST-1.
    move '96'           to VELOCIDAD-1.
    move nCodVd         to CODI-VEND-1
    move 0              to NUM-TELEFONO.
    move 0              to NUM-AREA.
    move '        '     to SUBMIT-FILE
    move nCodVd         to SUBMIT-FILE
    move '        '     to IDENTIF-FILE
    move nCodVd         to IDENTIF-FILE
;   move PUNT-COMU      to COMM-MODE
    move 0              to COMM-MODE
    write HOST-PARAMS-R.
    exit.

LLAMA2-RUTINA.
;------------
    move 'SUB710' to FILE-NAME.
    open REF PROGRAMA-F
         when ER clear
                 beep
                 message 1, ' Codigo No Esta '
                 wait 1 seconds
                 exit.
    call PROGRAMA-F
         when OK move 1 to carga-ok.
    move 'FILELIST' to FILE-NAME.
    erase FILELIST-F
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                            BORRA ARCHIVOS DE SALIDA                        ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
BORRA-DATOS.
;-----------
    beep.
    message  1, '****************'.
    message 17, 'Borra Movimiento'.
    message 33, '    Yes / No    '.
    message 49, '****************'.
    perform lee-tecla-action
        when ENTER perform BORRA-DATOS-001 exit.
        when NO    exit.
        when KEND  exit.
    loop.

BORRA-DATOS-001.
;---------------

    message  1, '****************'
    message 17, '    Borrando    '
    message 33, ' Movimientos... '
    message 49, '****************'

    perform Act_Fol_All

    move 1 to lFlgIvCs
    perform OPENUP-ARC-INV-F
    erase ARC-INV-F

    move 0 to lFlgIvCs
    perform OPENUP-ARC-INV-F
    erase ARC-INV-F

    perform OPENUP-ARC-NUE-F.
    erase ARC-NUE-F.

    perform OPENUP-RECARGA-F.
    erase RECARGA-F.

    perform OPENUP-DESCARGA-F.
    erase DESCARGA-F.

    perform OPENUP-ARC-CAR-F.
    erase ARC-CAR-F.

    perform OPENUP-PAGOSF
    erase PAGOSF-F

    perform OPENUP-ARC-ATE-F.
    erase ARC-ATE-F.

    perform OPENUP-CFA-VTA-F.
    erase CFA-VTA-F.

    perform OPENUP-DET-VTA-F.
    erase DFA-VTA-F.

    perform OPENUP-ENV-CSG-F.
    erase ENV-CSG-F.

    perform OPENUP-EVENTUALF.
    erase EVENTUALF-F.

    perform OPENUP-EVENTUALE.
    erase EVENTUALE-F.

    perform OPENUP-EVENTUALC
    erase EVENTUALC-F

    perform OPENUP-PRIN-01-F.
    erase PRIN-01-F.

    perform OPENUP-PRIN-02-F.
    erase PRIN-02-F.

    perform OPENUP-PRIN-04-F.
    erase PRIN-04-F.

    perform OPENUP-PRIN-05-F.
    erase PRIN-05-F.

    perform OPENUP-PRIN-06-F.
    erase PRIN-06-F.

    perform OPENUP-PRIN-07-F.
    erase PRIN-07-F.

    perform OPENUP-PRIN-08-F.
    erase PRIN-08-F.

    perform OPENUP-PRIN-09-F.
    erase PRIN-09-F.

    perform OPENUP-PRIN-10-F.
    erase PRIN-10-F.

    perform OPENUP-PRIN-11-F.
    erase PRIN-11-F.

    perform OPENUP-PRIN-12-F.
    erase PRIN-12-F.

    perform OPENUP-PRIN-13-F.
    erase PRIN-13-F.

    perform OPENUP-PRIN-14-F.
    erase PRIN-14-F.

    perform OPENUP-PRIN-15-F.
    erase PRIN-15-F.

    perform OPENUP-PRIN-16-F.
    erase PRIN-16-F.

    perform OPENUP-PRIN-17-F.
    erase PRIN-17-F.

    perform OPENUP-PRIN-18-F.
    erase PRINT-18-F.

    perform OPENUP-PRIN-19-F.
    erase PRIN-19-F.

    perform OPENUP-DEV-ENV-F.
    erase DEV-ENV-F.

    perform OPENUP-CAB-PRE-F
    erase CAB-PRE-F.

    perform OPENUP-DCO-PAG-F.
    erase DCO-PAG-F.

    perform OPENUP-ARC-CHE-F.
    erase ARC-CHE-F.

    perform OPENUP-CAB-VAR-F.
    erase CAB-VAR-F.

    perform OPENUP-DET-VAR-F.
    erase DET-VAR-F.

    perform OPENUP-CAB-CSG-F.
    erase CAB-CSG-F.

    perform OPENUP-DEV-CSG-F.
    erase DEV-CSG-F.

    perform OPENUP-DevCsg-F.
    erase DevCsg-F.

    perform OPENUP-PrEvCsg-F.
    erase PrEvCsg-F.

    perform OPENUP-ROTURAS-F.
    erase ROTURAS-F.

    perform OPENUP-OBSEQUIOS-F.
    erase OBSEQUIOS-F.

    perform OPENUP-GASTOS-F.
    erase GASTOS-F.

    perform OPENUP-CANJES-F
    erase CANJES-F
    exit.

Act_Fol_All.
;-----------
perform OPENREF-TAB-FOL-F
   when ER exit.
SEEK FIRST in TAB-FOL-F
      WHEN EOF exit.
      WHEN ER  exit.

Act_Fol_All001.
;--------------
READ TAB-FOL-F
move folio-tab-fol to folio-tab-ini
rewrite REG-TAB-FOL

SEEK NEXT in TAB-FOL-F
      WHEN EOF exit.
      WHEN ER  exit.
loop.


;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                         MENU ALMACENERO (BODEGERO)                         ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
MENU-ALMACENERO.
;---------------
move 1 to punt-mp

MENU-ALMAC-001.
;-------------
    move ' Menu Principal' to proc-ws.
    message 1,  '****************'.
    examine punt-mp
            when 0 move 3 TO punt-mp        loop.
            when 1 message 17, ' CARGA  INICIAL    DEL CAMION   '.
            when 2 message 17, '    RECARGA         EN RUTA     '.
            when 3 message 17, '   REPORTE DE      INVENTARIO   '.
            when 4 move 1 TO punt-mp        loop.
    message 49, '****************'.
    perform lee-tecla-fast-action
            when ENTER perform MENU-CHOICE.
            when FAR   add 1 to punt-mp.
            when BAR   subtract 1 from punt-mp.
            when KEND  perform SELECCIONA-MENU exit.
    loop.

MENU-CHOICE.
;-----------
    PERFORM LeeHoraFecha
    examine punt-mp
            when 1 perform INGRESO-CARGA-INI.
            when 2 perform RECARGA-PRO-RUTA.
            when 3 perform REPORTE-INVENTA.
    clear
    exit.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                  INGRESO CARGA INICIAL                  ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
INGRESO-CARGA-INI.
;-----------------

    perform LeeHoraFecha
    move 0 to lFlgFcCs
    move 0 to lFlgIvCs
    move 0 to sw-error
    move 0 to cCodPrd
    move 0 to d-producto-ws
    move 0 to p-producto-ws
    perform OPENREF-MAE-PRO-F
         when ER exit.
    seek first in MAE-PRO-F
      when EOF exit.
    read MAE-PRO-F

    perform OPENUP-PRIN-10-F
    reset PRIN-10-F

    move '11' to codigo-tab-fol
    perform lee-folio

    PERFORM ShowDocActual
            when ER exit.


    perform CARGA-INI-001
         when ER exit.
    perform imprime-carga
    add 1 to folio-tab-fol
    rewrite REG-TAB-FOL
    erase PRIN-10-F

    exit.

CARGA-INI-001.
;-------------
    move codigo-mae-pro to cCodPrd
    move descri-mae-pro to d-producto-ws
    move precio-mae-pro to p-producto-ws
    move nround-mae-pro to  nNroUnd
    show P-PRODUCTOS-SD
;   perform lee-tecla-fast-action

    move 0 to nCntPrd.
    accept CANT-PROD-SD
             when OK    if nCntPrd = 0 loop.
                        move nCntPrd to nCantidad
                        perform Iv_Normal_Entero
                        move nCantidad to nCntPrd

                        perform CARGA-INI-003
                             when ER exit.
                        perform AVANZA-CARGA
               end-when
             when DAR   perform AVANZA-CARGA.
             when UAR   perform RETROC-CARGA.
             when KEND  exit.
    loop.

AVANZA-CARGA.
;------------
    seek NEXT in MAE-PRO-F
         when EOF message 49, '-Final  archivo-'
                  beep
                  wait 1 second
                  seek last in MAE-PRO-F.
         when OK  read MAE-PRO-F.
    exit.

RETROC-CARGA.
;------------
    seek PRIOR in MAE-PRO-F
         when EOF message 49, '-Inicio archivo-'
                  beep
                  wait 1 second
                  seek first in MAE-PRO-F.
         when OK  read MAE-PRO-F.
    exit.

CARGA-INI-002.
;-------------
    move 0 to nCntPrd.
    accept CANT-PROD-SD
        when OK   exit.
        when KEND exit.
    loop.

CARGA-INI-003.
;------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '   Producto ?   '.
    message 49, '    Yes / No    '.
    perform lee-tecla-action.
             when NO    exit.
             when ENTER if sw-error = 0
                           perform OPENUP-ARC-INV-F
                           erase ARC-INV-F
                           move 1 to sw-error
                        end-if
                        perform OPENUP-ARC-INV-F
                        perform CARGA-INI-004
                             when ER exit.
                        perform CARGA-INI-005
                        perform GRABA-CARGA-PRIN-10
                        exit.
    loop.

CARGA-INI-004.
;-------------
    seek FIRST in ARC-INV-F
      when EOF move cCodPrd       to codigo-arc-inv
               move 0             to canti-arc-inv
               move nCntPrd       to ini-arc-inv
               move 0             to vco-arc-inv
               move 0             to vcr-arc-inv
               move 0             to pen-arc-inv
               move 0             to den-arc-inv
               move 0             to cso-arc-inv
               move 0             to csr-arc-inv
               move 0             to obs-arc-inv
               move 0             to cjs-arc-inv
               move 0             to rts-arc-inv
               move 0             to dsc-arc-inv
               move 0             to recarga-inv
              ;move nRuta         to rut-arc-inv
               move nFecha        to fec-arc-inv
               move nCodVd        to ven-arc-inv
               move x'0D'         to cr-inv
               move x'0A'         to lf-inv
               write REG-ARC-INV
               loop.

    search FIRST for cCodPrd in ARC-INV-F
        when OK read ARC-INV-F
                add nCntPrd to canti-arc-inv
                move 0      to vco-arc-inv
                move 0      to vcr-arc-inv
                move 0      to pen-arc-inv
                move 0      to cso-arc-inv
                move 0      to csr-arc-inv
                move 0      to den-arc-inv
                move 0      to obs-arc-inv
                move 0      to cjs-arc-inv
                move 0      to rts-arc-inv
                move 0      to dsc-arc-inv
                move 0      to recarga-inv
              ; move nRuta  to rut-arc-inv
                move nFecha to fec-arc-inv
                move nCodVd to  ven-arc-inv
                rewrite REG-ARC-INV.

        when ER move cCodPrd       to codigo-arc-inv
                move nCntPrd       to canti-arc-inv
                move nCntPrd       to ini-arc-inv
                move 0             to vco-arc-inv
                move 0             to vcr-arc-inv
                move 0             to pen-arc-inv
                move 0             to csr-arc-inv
                move 0             to cso-arc-inv
                move 0             to den-arc-inv
                move 0             to obs-arc-inv
                move 0             to cjs-arc-inv
                move 0             to rts-arc-inv
                move 0             to dsc-arc-inv
                move 0             to recarga-inv
               ;move nRuta         to rut-arc-inv
                move nFecha        to fec-arc-inv
                move nCodVd        to ven-arc-inv
                move x'0D'         to  cr-inv
                move x'0A'         to  lf-inv
                write REG-ARC-INV.
    exit.

CARGA-INI-005.
;-------------
    perform OPENUP-RECARGA-F
    move cCodPrd      to  CodPrd-rec
    move nCntPrd      to  CntPrd-rec
    move nNroDoc      to  NroDoc-rec
    move 0            to  Ruta-rec
    move nCodVd       to  Vend-rec
    move nFecha       to  fecha-rec
    move nHora        to  hora-rec
    move "01"         to  Tipo-rec
    move x'0D'        to  cr-rec
    move x'0A'        to  lf-rec
    write REG-RECARGA
exit.





GRABA-CARGA-PRIN-10.
;-------------------
   perform OPENUP-PRIN-10-F
   move folio-tab-fol to p-10-documento
   move cCodPrd to p-10-cod-prod
   move nCntPrd  to p-10-cnt-prod
   write REG-PRINT-10
exit.

IMPRIME-CARGA.
;------------
   perform OPENREF-PRIN-10-F
   when er exit.
   seek first in prin-10-f
   when eof exit.
   read prin-10-f

   move 0 to total-inve

   perform SETEA-IMPRESORA
           when ER exit.
            when KEND move OK to last-key exit.
   clear.
   message 1, '* IMPRIMIENDO *'.

   build CABECERA-01.   print. ;Razขn social
   build CABECERA-02.   print. ;direcciขn

   move 'COMPROBANTE DE CARGA INICIAL' to linea-ws
   build linea-print print
   build RECARGA-RD     print ;subtกtulo

   move nNroDoc       to nro-doc-cab-var
   build COMPR-VARIOS-01 print ;documento
   build RAYA    print ;raya
   build CABECERA-04     print ;subtกtulo

   perform imprime-carga-loop

   build PIE-PAGINA-01 print
   build PIE-PAGINA-02 print
   build FIN-LISTADO.   print.
   disconnect.                                 ; Termino Comunicaciones IP-40

exit.

imprime-carga-loop.
;-----------------
   search first for p-10-cod-prod in MAE-PRO-F
   when ok read MAE-PRO-F
           move p-10-cod-prod  to CAMPO1
           move descri-mae-pro to CAMPO2
           move p-10-cnt-prod  to CAMPO3
           build DETALLE-01 print
           add CAMPO3 to total-inve
   seek next in prin-10-f
   when eof exit.
   read prin-10-f
loop.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ              RECARGA PRODUCTOS EN LA RUTA               ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
RECARGA-PRO-RUTA.
;----------------
    move 1 to sw-error.
    move 0 to cCodPrd.
    move 0 to d-producto-ws.
    move 0 to p-producto-ws.
    perform OPENREF-MAE-PRO-F.
         when ER exit.
    seek first in MAE-PRO-F.
      when EOF exit.

    read MAE-PRO-F.

    move '11' to codigo-tab-fol
    perform lee-folio

    PERFORM ShowDocActual
             when ER exit.

    perform RECARGA-INI-001.
         when ER exit.
   ;perform imprime-carga
    exit.

RECARGA-INI-001.
;-------------
    move codigo-mae-pro to cCodPrd
    move descri-mae-pro to d-producto-ws.
    move precio-mae-pro to p-producto-ws.
    move nround-mae-pro to  nNroUnd

    show P-PRODUCTOS-SD.
    move 0 to nCntPrd.

    perform Muestra-Cant-Prod
            when ER exit.

    perform Rec_Verifica_item
            when OK READ PRIN-10-F
                    move p-10-cnt-prod to nCntPrd
                    show CANT-PROD-SD.

            when ER IF can-producto-ws = 0
                       BEEP
                       MESSAGE 49, 'No hay en Stock '
                    ELSE
                       MESSAGE 49, 'Stock : '
                       SHOW Solo-Hay-Sd
                    END-IF
            end-when

    perform lee-tecla-fast-action
             when ENTER perform Rec_Borra_Item
                        accept CANT-PROD-SD
                        move nCntPrd to nCantidad
                        perform Iv_Normal_Entero
                        move nCantidad to nCntPrd
                        if nCntPrd = 0 loop.
                           perform Rec_Confirma_Prod
                                when ER exit.
                        perform GRABA-RECARGA-PRIN-10
                        perform AVANZA-RECARGA
               end-when
             when DAR   perform AVANZA-RECARGA.
             when UAR   perform RETROC-RECARGA.
             when KEND  perform Rec_Verifica
                                when ER exit.
                        perform Rec_Confirma_Recarga
                                when ER loop.
                        perform Imprime-ReCarga
                        perform Rec_Graba
                        perform Rec_Actfolio

                        exit.
             when KELIM perform Rec_Borra_Item.

    loop.

Rec_Borra_Item.
;--------------
perform Rec_Verifica_Item
        when OK DELETE RECORD FROM PRIN-10-F.
exit.

Rec_ActFolio.
;-----------
move '11' to codigo-tab-fol
perform LEE-FOLIO
add 1  to  folio-tab-fol
rewrite REG-TAB-FOL
exit.


Rec_Graba.
;--------
    perform LeeHoraFecha
    perform OPENUP-PRIN-10-F
    search first for nNroDoc        in PRIN-10-F
        when ER exit.
        when EOF exit.
    read PRIN-10-F

    move p-10-Cod-Prod    to cCodPrd
    move p-10-cnt-prod    to nCntPrd
    DELETE RECORD FROM PRIN-10-F

    perform OPENUP-RECARGA-F
    move cCodPrd      to  CodPrd-rec
    move nCntPrd      to  CntPrd-rec
    move nNroDoc      to  NroDoc-rec
    move nRuta        to  Ruta-rec
    move nCodVd       to  Vend-rec
    move nFecha       to  fecha-rec
    move nHora        to  hora-rec
    move "02"         to  Tipo-rec
    move x'0D'        to  cr-rec
    move x'0A'        to  lf-rec
    write REG-RECARGA

    perform Iv_Add_Producto
    perform Rc_Add_Prd
    loop.


Rc_Add_Prd.
;-----------
perform OPENREF-ARC-INV
search FIRST for cCodPrd in ARC-INV-F
       when OK  read ARC-INV-F
                move recarga-inv     to nNumero1
                move nCntPrd         to nNumero2
                perform Iv_Suma_Enteros
                move nNumero1        to recarga-inv
                rewrite REG-ARC-INV
       end-when
exit.



Rec_Verifica_Item.
;-----------------
perform  OPENUP-PRIN-10-F
move nNroDoc       to Documento
move cCodPrd       to Producto
search first for Clave_Envases in PRIN-10-F
       when EOF move ER  to last-key.
exit.


Rec_Verifica.
;-----------
perform  OPENUP-PRIN-10-F
search FIRST for nNroDoc in  PRIN-10-F
       when EOF move ER to last-key.
exit.


Rec_Confirma_Recarga.
;-------------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '    Recarga     '.
    message 49, '    Yes / No    '.
    action

    when ENTER  move OK to last-key exit.
    when NO     move ER to last-key exit.
    loop.


AVANZA-RECARGA.
;------------
    seek NEXT in MAE-PRO-F
         when EOF message 49, '-Final  archivo-'
                  beep
                  wait 1 second
                  seek last in MAE-PRO-F.
         when OK  read MAE-PRO-F.
    exit.

RETROC-RECARGA.
;------------
    seek PRIOR in MAE-PRO-F
         when EOF message 49, '-Inicio archivo-'
                  beep
                  wait 1 second
                  seek first in MAE-PRO-F.
         when OK  read MAE-PRO-F.
    exit.

RECARGA-INI-002.
;-------------
    move 0 to nCntPrd.
    accept CANT-PROD-SD
        when OK   exit.
        when KEND exit.
    loop.

Rec_Confirma_Prod.
;-----------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '   Producto ?   '.
    message 49, '    Yes / No    '.
    perform lee-tecla-action.
             when NO    move ER to last-key exit.
             when ENTER move OK to last-key exit.

    loop.


GRABA-RECARGA-PRIN-10.
;-------------------
   perform OPENUP-PRIN-10-F
   move nNroDoc to p-10-documento
   move cCodPrd to p-10-cod-prod
   move nCntPrd to p-10-cnt-prod
   write REG-PRINT-10
exit.

IMPRIME-ReCARGA.
;------------
   perform OPENREF-PRIN-10-F
   when er exit.
   seek first in prin-10-f
   when eof exit.
   read prin-10-f

   move 0 to total-inve

   perform SETEA-IMPRESORA
           when ER exit.
            when KEND move OK to last-key exit.
   clear.
   message 1, '* IMPRIMIENDO *'.

   build CABECERA-01.   print. ;Razขn social
   build CABECERA-02.   print. ;direcciขn

   move '    COMPROBANTE DE RECARGA   ' to linea-ws
   build linea-print print
   build RECARGA-RD     print ;subtกtulo

   move nNroDoc       to nro-doc-cab-var
   build COMPR-VARIOS-01 print ;documento
   build RAYA    print ;raya
   build CABECERA-04     print ;subtกtulo

   perform imprime-Recarga-loop

   build PIE-PAGINA-01 print
   build PIE-PAGINA-02 print
   build FIN-LISTADO.   print.
   disconnect.                                 ; Termino Comunicaciones IP-40

exit.

imprime-recarga-loop.
;-----------------
   search first for p-10-cod-prod in MAE-PRO-F
   when ok read MAE-PRO-F
           move p-10-cod-prod  to CAMPO1
           move descri-mae-pro to CAMPO2
           move p-10-cnt-prod  to CAMPO3
           build DETALLE-01 print
           add CAMPO3 to total-inve
   seek next in prin-10-f
   when eof exit.
   read prin-10-f
loop.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                    DESCARGAS EN RUTA                    ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
DESCARGAS-ALM.
;-------------
move 1 to punt-car.

DESCAR-001.
;----------
    message 1,  '****************'.
    examine punt-car
            when 0 move 3 to punt-car       loop.
         ;  when 1 message 17 '    Raciones                    '.
            when 1 message 17 '    Roturas                     '.
            when 2 message 17 '     Gastos         en Ruta     '.
            when 3 message 17 '   Obsequios                    '.
            when 4 move 1 to punt-car       loop.
    message 49, '    Yes / No    '.
    perform lee-tecla-fast-action
            when ENTER perform DESCAR-002.
            when KEND  exit.
            when NO    add 1 to punt-car.
            when FAR   add 1 to punt-car.
            when BAR   subtract 1 from punt-car.
    loop.

DESCAR-002.
;----------
    examine punt-car
      ;     when 1 perform RACIONES.
            when 1 perform ROTURAS.
            when 2 perform GASTOS-RUTA.
            when 3 perform OBSEQUIOS.
    exit.

PARCIAL-DINERO.
;--------------
    exit.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                   ROTURAS EN RUTA                       ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
ROTURAS.
;-------
move '       ' to cNroDoc
move 0         to nCntPrd
move ''        to cCodPrd

move '12' to codigo-tab-fol             ;Codigo para Obsequios
perform LEE-FOLIO

PERFORM ShowDocActVen
        when ER exit.

perform OPENREF-MAE-PRO-F
        when ER exit.
seek first in MAE-PRO-F
     when EOF exit.
     read MAE-PRO-F
     perform RT-RECORRE-PRODUCTO.
             when ER exit.
   when KEND exit.
exit.

RT-RECORRE-PRODUCTO.
;-------------------
 move 0              to  nStock
 move codigo-mae-pro to  cCodPrd      ; Para la pantalla
 move nround-mae-pro to  nNroUnd      ;

 show CODIGO-PRODUCTO-SD
 show MUESTRA-DESC-SD

 perform Rt_Verifica_item
        when OK READ ROTURAS-F
                move rt-CntPrd to nCntPrd
                show CANT-PROD-SD.

        when ER perform VERIFICA-STK    ; Muestra Stock.
                        when ER exit.

        end-when


        ACTION
        when DAR   perform AVANZA-PRODUCTO.
        when UAR   perform RETROC-PRODUCTO.

        when ENTER if nStock = 0  loop.
                   perform Rt_Borra_Item
                   perform RT-ACEPTA-CANTIDAD
                           when ER loop.
                   perform RT-CONFIRMA-PRODUCTO
                           when ER exit.
                   perform RT-GRABA-ROTURA
                   perform AVANZA-PRODUCTO.

        when KEND  perform Rt_Verifica
                      when ER exit.
                   perform RT-CONFIRMA
                      when ER loop.
                   perform Rt_Graba
                   perform Rt_ActFolio
                   exit.
        when KELIM perform Rt_Borra_Item.


        loop.

Rt_Borra_Item.
;-------------
perform Rt_verifica_item
   when OK DELETE RECORD FROM ROTURAS-F loop.
exit.

Rt_Graba.
;--------
perform OPENUP-ROTURAS-F
search first for nNroDoc in ROTURAS-F
       when EOF exit.
       when ER exit.

Rt_Graba001.
;-----------
read ROTURAS-F
if nNroDoc <>  Rt-NroDoc  exit.
move Rt-nround     to nNroUnd
move Rt-codprd     to cCodPrd
move Rt-cntprd     to nCntPrd
perform Iv_Sub_Producto
perform Rt_Add_Inv
seek NEXT IN ROTURAS-F
     when EOF exit.
loop.


Rt_Add_Inv.
;-------------
perform OPENREF-ARC-INV
search FIRST for cCodPrd in ARC-INV-F
       when OK  read ARC-INV-F
                move rts-arc-inv     to nNumero1               ;Aumenta Cajas Vendidas Crdito
                move nCntPrd         to nNumero2
                perform Iv_Suma_Enteros
                move nNumero1        to rts-arc-inv
                rewrite REG-ARC-INV
       end-when
exit.



Rt_Verifica_Item.
;----------------
perform  OPENUP-ROTURAS-F
move nNroDoc       to Documento
move cCodPrd       to Producto
search first for Clave_Envases in ROTURAS-F
       when EOF move ER  to last-key.
exit.


Rt_ActFolio.
;-----------
move '12' to codigo-tab-fol
perform LEE-FOLIO
add 1  to  folio-tab-fol
rewrite REG-TAB-FOL
exit.

Rt_Verifica.
;-----------
perform  OPENUP-ROTURAS-F
search FIRST for nNroDoc in ROTURAS-F
       when EOF move ER to last-key.
exit.


RT-ACEPTA-CANTIDAD.
;------------------
move 0      to nCntPrd
accept CANTIDAD-SD

       when ER  loop.
       when OK  move nCntPrd to nCantidad
                perform Iv_Normal_Entero
                move nCantidad to nCntPrd
                if nCntPrd > nStock loop.
                if nCntPrd = 0  move ER to last-key.
                exit.
       when KEND  move ER TO LAST-KEY exit.

       end-when
loop.

RT-CONFIRMA.
;-----------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '     Rotura     '.
    message 49, '    Yes / No    '.
    action

    when ENTER  move OK to last-key exit.
    when NO     move ER to last-key exit.
    loop.

RT-CONFIRMA-PRODUCTO.
;--------------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '    Producto    '.
    message 49, '    Yes / No    '.
    action
             when ENTER  move OK to last-key exit.
             when KEND   move ER to last-key exit.
             when NO     move ER to last-key exit.
    loop.

RT-GRABA-ROTURA.
;--------------
perform  OPENUP-ROTURAS-F
move nNroDoc         to   rt-nrodoc
move cCodPrd         to   rt-CodPrd
move nCntPrd         to   rt-CntPrd
move nFecha          to   rt-Fecha
move nRuta           to   rt-Ruta
move nNroUnd         to   rt-NroUnd
move x'0D'           to   rt-cr
move x'0A'           to   rt-lf

write REG-ROTURAS
exit.


;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                   OBSEQUIOS EN RUTA                     ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
OBSEQUIOS.
;----------
move 0         to nNroDoc
move 0         to nCntPrd
move ''        to cCodPrd
move '07' to codigo-tab-fol             ;Codigo para Obsequios
perform LEE-FOLIO

PERFORM ShowDocActVen
        when ER exit.

perform OPENREF-MAE-PRO-F
        when ER exit.
seek first in MAE-PRO-F
     when EOF exit.
     read MAE-PRO-F
     perform OB-RECORRE-PRODUCTO
             when ER exit.
   when KEND exit.
exit.

OB-RECORRE-PRODUCTO.
;-------------------
 move 0              to  nStock
 move codigo-mae-pro to  cCodPrd      ; Para la pantalla
 move nround-mae-pro to  nNroUnd      ;
 show CODIGO-PRODUCTO-SD
 show MUESTRA-DESC-SD

 perform Ob_Verifica_item
        when OK READ OBSEQUIOS-F
                move Ob-CntPrd to nCntPrd
                show CANT-PROD-SD.

        when ER perform VERIFICA-STK    ; Muestra Stock.
                        when ER exit.

        end-when

 ACTION
 when DAR   perform AVANZA-PRODUCTO.
 when UAR   perform RETROC-PRODUCTO.
 when ENTER if nStock = 0  loop.
            perform Ob_Borra_Item
            perform OB-ACEPTA-CANTIDAD
                    when ER loop.
            perform OB-CONFIRMA
                    when ER loop.
            perform OB-GRABA
            perform AVANZA-PRODUCTO.

 when KEND  perform Ob_Verifica
                    when ER exit.
                    when OK perform OB-CONFIRMA-OBSEQUIO
                            when ER loop.
                            when OK perform Ob_Graba
                                    perform Ob-Imprimir
                                    perform Ob_ActFolio
                                    exit.
                    end-when
            exit.

 when KELIM  perform Ob_Borra_Item.

 loop.

Ob_Borra_Item.
;-------------
perform Ob_Verifica_item
   when OK DELETE RECORD FROM OBSEQUIOS-F.
exit.

Ob_Graba.
;--------
perform OPENUP-OBSEQUIOS-F
search first for nNroDoc in OBSEQUIOS-F
       when EOF exit.
       when ER exit.

Ob_Graba001.
;-----------
read OBSEQUIOS-F
if nNroDoc <>  Ob-NroDoc  exit.
move Ob-nround     to nNroUnd
move Ob-codprd     to cCodPrd
move Ob-cntprd     to nCntPrd
perform FC-DESCUENTA-INV
perform Ob_Add_Inv
seek NEXT IN OBSEQUIOS-F
     when EOF exit.
loop.





Ob_Verifica_Item.
;----------------
perform  OPENUP-OBSEQUIOS-F
move nNroDoc       to Documento
move cCodPrd       to Producto
search first for Clave_Envases in OBSEQUIOS-F
       when EOF move ER  to last-key.
exit.

Ob_Add_Inv.
;-------------
perform OPENREF-ARC-INV
search FIRST for cCodPrd in ARC-INV-F
       when OK  read ARC-INV-F
                move obs-arc-inv     to nNumero1               ;Aumenta Cajas Vendidas Crdito
                move nCntPrd         to nNumero2
                perform Iv_Suma_Enteros
                move nNumero1        to obs-arc-inv
                rewrite REG-ARC-INV
       end-when
exit.

Ob_Verifica.
;-----------
perform  OPENUP-OBSEQUIOS-F
search FIRST for nNroDoc in  OBSEQUIOS-F
       when EOF move ER to last-key.
exit.

OB-ACEPTA-CANTIDAD.
;------------------
move 0      to nCntPrd
accept CANTIDAD-SD

when ER  loop.
when OK  move nCntPrd to nCantidad
         perform Iv_Normal_Entero
         move nCantidad to nCntPrd
         if nCntPrd > nStock loop.
         if nCntPrd = 0  move ER to last-key.
         exit.
when KEND MOVE ER TO LAST-KEY exit.

LOOP.

OB-CONFIRMA-OBSEQUIO.
;-------------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '    Obsequio    '.
    message 49, '    Yes / No    '.
    action

    when ENTER  move OK to last-key exit.
    when NO     move ER to last-key exit.
    loop.

OB-CONFIRMA.
;-------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '    Producto    '.
    message 49, '    Yes / No    '.
    action
             when ENTER  move OK to last-key exit.
             when KEND   move ER to last-key exit.
             when NO     move ER to last-key exit.
    loop.

OB-GRABA.
;-------
perform  OPENUP-OBSEQUIOS-F
move nNroDoc         to   Ob-nrodoc
move cCodPrd         to   Ob-CodPrd
move nCntPrd         to   Ob-CntPrd
move nFecha          to   Ob-Fecha
move nRuta           to   Ob-Ruta
move nNroUnd         to   Ob-NroUnd
move x'0D'           to   Ob-cr
move x'0A'           to   Ob-lf
write REG-OBSEQUIOS
exit.

Ob_ActFolio.
;-----------
move '07' to codigo-tab-fol
perform LEE-FOLIO
add 1  to  folio-tab-fol
rewrite REG-TAB-FOL
exit.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ Imprime  Obsequios.                                                       ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

Ob-IMPRIMIR.
;-----------
    perform SETEA-IMPRESORA
            when ER exit.
            when KEND move OK to last-key exit.
    clear.
    message 1, '* IMPRIMIENDO *'.
    next.

OB-IMPRIMIR-OBSEQUIO.
;---------------------
    search FIRST for nNroDoc in OBSEQUIOS-F
    build CAB-FACT-000     print
    build CAB-OB-001       print
    build CAB-OB-002       print
    perform OB-IMPRIMIR-SIGUIENTE
    build FIRMA-VEND-CLIENTE print
    build FIN-LISTADO      print
    disconnect                                  ; Termino Comunicaciones IP-40
    exit.

OB-IMPRIMIR-SIGUIENTE.
;---------------------
read OBSEQUIOS-F
move Ob-CodPrd to busca-pro-ws
perform BUSCA-COD-PROD

if Ob-NroDoc =  nNroDoc
   build CAB-OB-003    print
   seek NEXT in OBSEQUIOS-F
     when EOF exit.
   loop
end-if
exit.



;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ Gastos de Vendedor en Ruta.                                               ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
GASTOS-RUTA.
;-----------
move '13' to codigo-tab-fol
perform Lee-folio

PERFORM ShowDocActVen
        when ER exit.


   MOVE 0      TO nCntDin
   accept  DESC-DINERO-SD.

        when ER   loop.
        when OK   if nCntDin = 0  loop.
                  perform GS-VERIFICA-CAJA
                       when ER loop.

                  perform GS-CONCEPTO-GASTO
                       when ER loop.

                  perform GS-GRABA.
                  perform GS-RESTA-CAJA
                       when ER exit.
                  perform Gs_ActFolio
                  exit
          end-when
        when KEND exit.
    loop.

GS-GRABA.
;-----------
perform  OPENUP-GASTOS-F
move nNroDoc         to   gs-documento
move cConcepto       to   gs-Concepto
move nCntDin         to   gs-CntDin
move x'0D'           to   gs-cr
move x'0A'           to   gs-lf
write REG-GASTOS
exit.

GS-CONCEPTO-GASTO.
;-----------------

accept  GS-CONCEPTO-SD
  when KEND exit.
  when OK   exit.
  when ER   loop.
loop.

GS-VERIFICA-CAJA.
;----------------
    perform OPENREF-ARC-CAR-F
    seek FIRST in ARC-CAR-F
      when EOF exit.
    read ARC-CAR-F
    if caja-arc-car < nCntDin
       beep
       message 01, '****************'
       message 17, '*No Existe esa *'
       message 33, '*   Cantidad   *'
       message 49, '****************'
       wait 1 second
       move ER to last-key
    else
       move OK to last-key
    end-if
    exit.

GS-RESTA-CAJA.
;----------------
    perform OPENUP-ARC-CAR-F
    seek FIRST in ARC-CAR-F
      when EOF exit.
    read ARC-CAR-F
    subtract nCntDin from caja-arc-car
    rewrite REG-ARC-CAR
    exit.

Gs_ActFolio.
;-----------
move '13' to codigo-tab-fol
perform LEE-FOLIO
add 1  to  folio-tab-fol
rewrite REG-TAB-FOL
exit.


;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ       Canjes de Envases.                                                 ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

CANJES.
;------
   move 0         to nCntPrd
   move 0         to cCodPrd

   perform OPENREF-MAE-PRO-F
           when ER exit.
   seek first in MAE-PRO-F
           when EOF exit.
   READ MAE-PRO-F

   move 1  to punt-canjes.
   perform CjMueveFolio
   perform ShowDocActual
   perform CJ-ELIGE-RAZON
           when ER exit.
   perform RECORRE-PRODUCTO

exit.

RECORRE-PRODUCTO.
;-------------------
 move 0              to  nStock
 move codigo-mae-pro to  cCodPrd         ; Variable Estandar.
 move nround-mae-pro to  nNroUnd      ; Para la pantalla

 show CODIGO-PRODUCTO-SD                 ; Muestra Cขdigo.
 show MUESTRA-DESC-SD                    ; Muestra descripciขnd del Producto.

 perform Cj_Verifica_item
        when OK READ PRIN-14-F
                move p-14-cnt-prod to nCntPrd
                show CANT-PROD-SD.

        when ER perform VERIFICA-STK    ; Muestra Stock.
                        when ER exit.

        end-when

 perform lee-tecla-action
         when ENTER if nStock = 0  loop.
                    perform Cj_Borra_Item
                    perform Cj_ACEPTA_CANTIDAD
                            when ER loop.
                    perform CONFIRMA-PRD
                            when ER loop.
                    perform CJ_Graba_Item
                    perform AVANZA-PRODUCTO.

         when DAR   perform AVANZA-PRODUCTO.
         when UAR   perform RETROC-PRODUCTO.
         when KELIM perform Cj_Borra_Item.
         when KEND  perform Cj_Verifica
                       when ER exit.
                    perform CJ-CONFIRMA
                       when ER perform Cj_Borra_Todo loop.
                    perform imprime-canjes
                    perform  Cj_Actualiza_Folio
                    perform Cj_Graba
                    exit
              end-when
 loop.


Cj_Borra_Item.
;-------------
perform Cj_verifica_item
        when OK DELETE RECORD FROM PRIN-14-F loop.
exit.

CjMueveFolio.
;--------------
 move  05  to codigo-tab-fol
 perform LEE-FOLIO
 exit.

Cj_Actualiza_Folio.
;------------------
    move '05' to codigo-tab-fol
    search FIRST for codigo-tab-fol in TAB-FOL-F
    add 1  to folio-tab-fol
    rewrite REG-TAB-FOL
    exit.


CJ-ELIGE-RAZON.
;-------------
    message 1,  '****************'
    message 33, '    Yes / No    '
    examine punt-canjes
            when 0 move 3 TO punt-canjes    loop.
            when 1 message 17, ' Bot. Mermadas  '.
            when 2 message 17, ' Bot.Despicadas '.
            when 3 message 17, '    Otros       '.
            when 4 move 1 TO punt-canjes    loop.
    message 49, '****************'
    fast-action
            WHEN ENTER move punt-canjes to nRazon
                       exit.
            WHEN FAR   add 1 to punt-canjes.
            WHEN NO    add 1 to punt-canjes.
            WHEN BAR   subtract 1 from punt-canjes.
            WHEN KEND  move ER to last-key exit.
    loop.

Cj_Graba.
;--------
perform OPENREF-PRIN-14-F
search first for nNroDoc in PRIN-14-F
       when EOF exit.
       when ER exit.
       when OK read PRIN-14-F.

perform  OPENUP-CANJES-F
move p-14-documento    to   cj-NroDoc
move p-14-razon        to   cj-Razon
move nRuta             to   cj-Ruta
move p-14-cod-prod     to   cj-CodPrd
move p-14-cnt-prod     to   cj-CntPrd
move p-14-cod-cli      to   cj-CodCli
move p-14-date         to   cj-Fecha
move x'0D'             to   cj-cr
move x'0A'             to   cj-lf
write REG-CANJES

move p-14-und-prod     to nNroUnd
move p-14-cod-prod     to cCodPrd
move p-14-cnt-prod     to nCntPrd
perform Iv_Sub_Producto
perform Cj_Add_Inv
perform Cj_Borra
loop.

Cj_Add_Inv.
;-------------
perform OPENREF-ARC-INV
search FIRST for cCodPrd in ARC-INV-F
       when OK  read ARC-INV-F
                move cjs-arc-inv     to nNumero1               ;Aumenta Cajas Vendidas Crdito
                move nCntPrd         to nNumero2
                perform Iv_Suma_Enteros
                move nNumero1        to cjs-arc-inv
                rewrite REG-ARC-INV
       end-when
exit.


Cj_Borra.
;--------
perform OPENUP-PRIN-14-F
search first for nNroDoc in PRIN-14-F
       when OK DELETE RECORD FROM PRIN-14-F.
exit.

Cj_Borra_todo.
;-------------
perform Cj_Borra
       when OK loop.
exit.


Cj_Verifica_Item.
;----------------
perform  OPENUP-PRIN-14-F
move nNroDoc       to Documento
move cCodPrd       to Producto
search first for Clave_Envases in PRIN-14-F
       when EOF move ER  to last-key.
exit.


Cj_Verifica.
;-----------
perform  OPENUP-PRIN-14-F
search FIRST for nNroDoc in  PRIN-14-F
       when EOF move ER to last-key.
exit.


ACEPTA-PRODUCTO.
;---------------
    move 0 to cCodPrd
    accept CODIGO-PRODUCTO-SD
        when ER    loop.
        when OK   ;if cCodPrd = 0 loop.
                   move cCodPrd to busca-pro-ws
                   perform BUSCA-COD-PROD
                   exit
          end-when
        when KEND  exit.
        when UAR   exit.
        when DAR   exit.
    loop.


CJ-CONFIRMA.
;-----------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '     Canjes     '.
    message 49, '    Yes / No    '.
    action

    when ENTER  move OK to last-key exit.
    when NO     move ER to last-key exit.

    loop.


Cj_ACEPTA_CANTIDAD.
;------------------
move 0      to nCntPrd
accept CANTIDAD-SD

     when ER  loop.
     when OK  move nCntPrd to nCantidad
              perform Iv_Normal_Entero
              move nCantidad to nCntPrd
              if nCntPrd > nStock loop.
              if nCntPrd = 0 move ER to last-key.
              exit.
     when KEND move ER to last-key  exit.
LOOP.

CONFIRMA-PRD.
;-------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '    Producto    '.
    message 49, '    Yes / No    '.
    action
             when ENTER  move OK to last-key exit.
             when KEND   move ER to last-key exit.
             when NO     move ER to last-key exit.
    loop.

Cj_Graba_Item.
;------------
perform OPENUP-PRIN-14-F.
perform Cj_Verifica_Item
    when OK DELETE RECORD FROM PRIN-14-F.

move  nCodCli           to   p-14-cod-cli
move  cNomcli           to   p-14-nom-cli
move  cDirCli           to   p-14-dir-cli
move  nNroDoc           to   p-14-documento
move  nFecha            to   p-14-date
move  cCodPrd           to   p-14-cod-prod
move  nCntPrd           to   p-14-cnt-prod
move  descri-mae-pro    to   p-14-dsc-prod
move  nNroUnd           to   p-14-und-prod
move  nRazon            to   p-14-razon
write REG-PRINT-14
exit.


Imprime-Canjes.
;--------------
   perform OPENREF-PRIN-14-F
           when er exit.

   perform SETEA-IMPRESORA
           when ER exit.
            when KEND move OK to last-key exit.
   clear
   message 1, '* IMPRIMIENDO *'

   build CABECERA-01.   print. ;Razขn social
   build CABECERA-02.   print. ;direcciขn

   move '           COMPROBANTE DE CANJE           ' to linea-ws
   build linea-print print

   move nNroDoc       to nro-doc-cab-var
   build COMPR-VARIOS-01 print ;documento
   move 1 to nRazon
   move '            Botellas Mermadas             ' to linea-ws
   perform IMPRIME-CANJES-RAZON

   move 2 to nRazon
   move '           Botellas Despicadas            ' to linea-ws
   perform IMPRIME-CANJES-RAZON

   move 3 to nRazon
   move '                  Otros                   ' to linea-ws
   perform IMPRIME-CANJES-RAZON

   build FIRMA-VEND-CLIENTE print
   build FIN-LISTADO        print.
   disconnect.                                 ; Termino Comunicaciones IP-40
exit.

IMPRIME-CANJES-RAZON.
;------------------
    search FIRST for nNroDoc in PRIN-14-F
    when ER exit.
    READ PRIN-14-F
    if nRazon <> p-14-razon or nNroDoc <> p-14-documento
       perform Cj_Avanza_Razon
               when EOF exit.
    end-if
    build linea-print print
    lprint '       Descripcion         Cantidad'
    perform impr-canjes-razon-loop
exit.

impr-canjes-razon-loop.
;---------------------
    search first for p-14-cod-prod in mae-pro-f
    when OK read mae-pro-f
            move p-14-cnt-prod to canti-det-var
            build COMPR-VARIOS-05 print ;detalle de productos
    end-when

    perform Cj_Avanza_Razon
           when EOF exit.
           loop.
loop.

Cj_Avanza_Razon.
;---------------
 seek NEXT in PRIN-14-F
      when EOF exit.
 read PRIN-14-F
 if nRazon <> p-14-razon or nNroDoc <> p-14-documento
    loop.
 exit.


;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                                 RACIONES                                  ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

RACIONES.
;--------
move 0         to nNroDoc
move 0         to nCntPrd
move ''        to cCodPrd

ACCEPT NRO-TICKET-SD

   when ER loop.
   when OK if nNroDoc = 0 loop.
           perform OPENREF-MAE-PRO-F
                   when ER exit.
           seek first in MAE-PRO-F
           when EOF exit.
           read MAE-PRO-F
           perform RC-RECORRE-PRODUCTO.
                   when ER exit.
   when KEND exit.
exit.

RC-RECORRE-PRODUCTO.
;-------------------
 move 0              to  nStock
 move codigo-mae-pro to  cCodPrd
 show RC-CODIGO-PRODUCTO-SD
 show MUESTRA-DESC-SD
 perform  VERIFICA-STK             ; Muestra Stock.

 ACTION
 when DAR   perform AVANZA-PRODUCTO.
 when UAR   perform RETROC-PRODUCTO.
 when ENTER if nStock = 0  loop.
            perform RC-ACEPTA-CANTIDAD.
 when KEND
            perform RC-CONFIRMA-RAC
               when ER loop.
               when OK exit.
      end-when
 loop.

RC-ACEPTA-PRODUCTO.
;---------------
    move 0 to cCodPrd
    accept RC-CODIGO-PRODUCTO-SD
        when OK    if cCodPrd = 0 loop.
                      move cCodPrd to busca-pro-ws
                      perform BUSCA-COD-PROD
                      exit
          end-when
        when KEND  exit.
        when UAR   exit.
        when DAR   exit.
    loop.



RC-CONFIRMA-RAC.
;---------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '     Envase     '.
    message 49, '    Yes / No    '.
    action

    when ENTER  move OK to last-key exit.
    when NO     move ER to last-key exit.

    loop.


RC-ACEPTA-CANTIDAD.
;------------------
move 0      to nCntPrd
accept CANTIDAD-SD
       when ER  loop.
       when OK  if nCntPrd = 0  exit.

perform RC-CONFIRMA
    when ER exit.
perform RC-GRABA-RACION
perform RC-UPDATE-STOCK
perform AVANZA-PRODUCTO
exit.

RC-CONFIRMA.
;-------------
    message  1, '****************'.
    message 17, '    Confirma    '.
    message 33, '    Producto    '.
    message 49, '    Yes / No    '.
    action
             when ENTER  move OK to last-key exit.
             when KEND   move ER to last-key exit.
             when NO     move ER to last-key exit.
    loop.


RC-GRABA-RACION.
;--------------
perform  OPENUP-RACIONES-F
move nNroDoc         to   rc-nrodoc
move cCodPrd to   rc-codpro
move nCntPrd         to   rc-cantidad
move x'0D'           to   rc-cr
move x'0A'           to   rc-lf

write REG-RACIONES
exit.

RC-UPDATE-STOCK.
;---------------
 perform OPENREF-ARC-INV.
      when ER exit.
 seek FIRST in ARC-INV-F.
   when EOF exit.
 search FIRST for cCodPrd in ARC-INV-F
     when ER  move canti-arc-inv to codigo-arc-inv
              move canti-arc-inv to canti-arc-inv
                 move x'0D'         to  cr-inv
                 move x'0A'         to  lf-inv
              write REG-ARC-INV
              exit.
 read ARC-INV-F
 subtract nCntPrd  from   canti-arc-inv
 rewrite REG-ARC-INV
 exit.


;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ               REPORTE DE CARGA DEL CAMION               ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
REPORTE-INVENTA.
;---------------
    perform SETEA-IMPRESORA
            when ER exit.
            when KEND move OK to last-key exit.
    clear.
    message 1, '* IMPRIMIENDO *'.
    perform OPENREF-ARC-INV
         when ER exit.
    next.

IMP-INVE-001.
    build CABECERA-01.   print.                 ; Imprime CABECERAS
    build CABECERA-02.   print.
    build CABECERA-03.   print.
    build CABECERA-04.   print.
    move 0 to total-inve.
    next.

IMP-INVE-002.
    read ARC-INV-F.
    move codigo-arc-inv to CAMPO1.
    move canti-arc-inv  to CAMPO3.
    add canti-arc-inv to total-inve.
    move codigo-arc-inv to busca-pro-ws.
    perform BUSCA-COD-PROD
         when ER exit.
    move descri-mae-pro to CAMPO2.
    build DETALLE-01.    print.
    seek NEXT in ARC-INV-F.
      when EOF next.
    loop.

IMP-INVE-003.
    build PIE-PAGINA-01. print.                 ; Imprime Pie de Pagina
    build PIE-PAGINA-02. print.
    build FIN-LISTADO.   print.
    disconnect.                                 ; Termino Comunicaciones IP-40
    exit.
;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                           AJUSTE DEL TONO DE TECLAS                        ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
AJUSTA-TONO.
;-----------
    examine tecla-tono
         when 00  key-click is 1   move  1 to tecla-tono.
         when 01  key-click is 2   move  2 to tecla-tono.
         when 02  key-click is 3   move  3 to tecla-tono.
         when 03  key-click is 4   move  4 to tecla-tono.
         when 04  key-click is 5   move  5 to tecla-tono.
         when 05  key-click is 6   move  6 to tecla-tono.
         when 06  key-click is 7   move  7 to tecla-tono.
         when 07  key-click is 8   move  8 to tecla-tono.
         when 08  key-click is 9   move  9 to tecla-tono.
         when 09  key-click is 10  move 10 to tecla-tono.
         when 10  key-click is 11  move 11 to tecla-tono.
         when 11  key-click is 12  move 12 to tecla-tono.
         when 12  key-click is 13  move 13 to tecla-tono.
         when 13  key-click is 14  move 14 to tecla-tono.
         when 14  key-click is 15  move 15 to tecla-tono.
         when 15  key-click is 16  move 16 to tecla-tono.
         when 16  key-click is 17  move 17 to tecla-tono.
         when 17  key-click is 18  move 18 to tecla-tono.
         when 18  key-click is 19  move 19 to tecla-tono.
         when 19  key-click is 20  move 20 to tecla-tono.
         when 20  key-click is 21  move 21 to tecla-tono.
         when 21  key-click is 22  move 22 to tecla-tono.
         when 22  key-click is 23  move 23 to tecla-tono.
         when 23  key-click is 24  move 24 to tecla-tono.
         when 24  key-click is 25  move 25 to tecla-tono.
         when 25  key-click is  0  move  0 to tecla-tono.
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                                CHECK-MEMORY                                ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
CHECK-MEMORY.
;-------------
    clear
    move DISPLAY-BUFFER to DISPLAY-SAVE
    if 2000 < MEM-AVAIL
       move 2000 to MEM-PROTECT
       message 1, 'MEM. DISPONIBLE '
       move OK to MEMORY-FLAG
       move MEM-AVAIL to WS-MEM-AVAIL
       show MEM-REM-S
    else
       if 200 < MEM-AVAIL
          move 200 to MEM-PROTECT
          message 1, ' FALTA  MEMORIA '
          move OK to MEMORY-FLAG
          move MEM-AVAIL to WS-MEM-AVAIL
          show MEM-REM-S
          beep
       else
          move 200 to MEM-PROTECT
          message 1, ' MEMORIA  LLENA '
          move ER to MEMORY-FLAG
          move MEM-AVAIL to WS-MEM-AVAIL
          show MEM-REM-S
    end-if
    wait 1 second
    move DISPLAY-SAVE to DISPLAY-BUFFER
    exit.



;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                           AJUSTE DEL SONIDO BEEP                           ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
AJUSTA-BEEP.
;-----------
    examine tecla-beep
         when 0 volume is quiet  move 3 to tecla-beep.
         when 1 volume is soft   move 0 to tecla-beep.
         when 2 volume is medium move 1 to tecla-beep.
         when 3 volume is loud   move 2 to tecla-beep.
    exit.


;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                                 CONTRASTE                                  ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
TURN-LIGHT-ON-OFF.
;-----------------
    if flag-hora = 0 then
       backlight-on 25 seconds
       move 1 to flag-hora
    else
       backlight-off
       move 0 to flag-hora.
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                                 RUTINAS DE PANTALLA                        ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
SaveScreen.
;----------
move DISPLAY-BUFFER to resp-pantalla
exit.

RestScreen.
;-----------
move resp-pantalla to DISPLAY-BUFFER.
exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ        RUTINA PARA SELECCIONAR MENU PRINCIPAL DE ACUERDO A USUARIO         ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
SELECCIONA-MENU.
;---------------
    move DISPLAY-BUFFER to resp-pantalla
    move 1 to prende-apaga.
    perform OPENREF-IDENTIF-F
         when ER  perform CARGA-DATOS-ALL
                  loop.
    seek FIRST in IDENTIFICA-F
         when EOF message  1, '****************'
                  message 17, 'Archivo Identif.'
                  message 33, '     Vacio      '
                  message 49, '****************'
                  perform lee-tecla-action
                  perform CARGA-DATOS-ALL
                  loop.
    read IDENTIFICA-F.
    move cod-arc-ide to nCodVd      .

SELECCIONA-001.
;--------------
    move 0         to ws-prende-ptc
    move 4         to prende-ws-1
    move ' '       to shift-char
    message 01  '*  EXCEL S.A.   *'.
    message 17, '****************'.
    message 33, ' Ingrese  Clave '.
    message 49  '                '.
    accept PRENDE-PTC-SD
       when OK perform LeeHoraFecha
               seek FIRST in IDENTIFICA-F
               search FIRST for ws-prende-ptc in IDENTIFICA-F offset is password-ide
                   when ER beep
                           message 01, '****************'
                           message 17, '**   Acceso   **'
                           message 33, '**  Denegado  **'
                           message 49, '****************'
                           wait 1 second
                           loop.
                   when OK read IDENTIFICA-F
                           if prende-ws-1 = 0
                              move cod-arc-ide to nCodVd
                              move nom-arc-ide to nom-vende-ws
                           end-if
                           move resp-pantalla to DISPLAY-BUFFER
                           next.
            end-when
    loop.

SELECCIONA-002.
;--------------
    move '@' to shift-char.
    move 0   to prende-apaga.
    clear.
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                        RUTINA PARA SETEAR IMPRESORA                        ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
SETEA-IMPRESORA.
;---------------
 ;move ER to last-key
 ;exit.
    clear                                       ; Confirma si esta
    message  1, ' * Impresora *  '              ; conectada la impresora
    message 17, '  Conectada ?   '
    message 49, '   Yes / No     '
    perform lee-tecla-action
           when  ENTER connect PRINTER-COM-40
                       when ER clear
                               prompt 1 'Error conectando   Printer'
                               disconnect.
                        exit
           end-when
           when  NO    loop.
      ;    when  NO    move ER to last-key exit.
           when  KEND  exit.
    loop.
;ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
;บ                           RUTINAS GENERALES                               บ
;ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
LeeHoraFecha.                            ; Carga en variebles Hora y Fecha.
;------------
  read-clock                             ; Fecha
  move SY-DATE-TIME to fecha-ws
  move dia-ws  to dia-normal-ws
  move mes-ws  to mes-normal-ws
  move ano-ws  to ano-normal-ws
  move hrs-ws  to hrs-normal-ws
  move minu-ws to min-normal-ws
  move sec-ws  to seg-normal-ws
  move dow-ws  to dow-normal-ws
  move dia-normal-ws to dia-emisi-ws
  move mes-normal-ws to mes-emisi-ws
  move ano-normal-ws to ano-emisi-ws
  exit.

Lee-Folio.
;---------
;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ Pasar en  CODIGO-TAB-FOL el Cขdigo de Movimiento                          ณ
;ณ Devuelve en  nNroDoc el documento actual.                                 ณ
;ณ                                                                           ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
    perform OPENUP-TAB-FOL-F
    search FIRST for codigo-tab-fol in TAB-FOL-F
        when ER  move nCodVd     to nSerie
                 move 1          to nCorrelativo
                 move nDocumento to  folio-tab-fol
                 move nDocumento to  folio-tab-ini
                 perform Asigna_literal_Folio
      ;          move x'0D' to  cr-folio
      ;          move x'0A' to  lf-folio
                 write REG-TAB-FOL
                 seek last in TAB-FOL-F.
        when OK  read TAB-FOL-F.
    move folio-tab-fol to nNroDoc
    exit.

Asigna_literal_Folio.
;--------------------
if codigo-tab-fol = 1
   move  "Facturas" to folio-tab-dsc.
if codigo-tab-fol = 2
   move  "Prestamos   " folio-tab-dsc.
if codigo-tab-fol = 3
   move  "Devoluciones" folio-tab-dsc.
if codigo-tab-fol = 4
   move  "Cobranzas   " folio-tab-dsc.
if codigo-tab-fol = 5
   move  "Canjes    " folio-tab-dsc.
if codigo-tab-fol = 6
   move  "Varios" folio-tab-dsc.
if codigo-tab-fol = 7
   move  "Obsequios" folio-tab-dsc.
if codigo-tab-fol = 8
   move  "Entrega de Dinero" folio-tab-dsc.
if codigo-tab-fol = 9
   move  "Descargas" folio-tab-dsc.
if codigo-tab-fol = 10
   move  "Boletas" folio-tab-dsc.
if codigo-tab-fol = 11
   move  "Recarga en Ruta" folio-tab-dsc.
if codigo-tab-fol = 12
   move  "Rotura" folio-tab-dsc.
if codigo-tab-fol = 13
   move  "Gastos" folio-tab-dsc.
if codigo-tab-fol = 14
   move  "Consig. Otorga" folio-tab-dsc.
if codigo-tab-fol = 15
   move  "Consig.Recogid" folio-tab-dsc.
if codigo-tab-fol = 16
   move  "Env. Rec. Csg." folio-tab-dsc.
if codigo-tab-fol = 17
   move  "Env. Dev. Csg." folio-tab-dsc.
exit.



ShowDocActual.
;-------------
    clear
    if prende-ws-1 = 0
       show COD-CLIEN-SD 
    end-if
    show FECHA-EMISI-SD 

    if lFlgFcCs = 1
       show NRO-DOCCSG-SD
    end-if

ShowDocActual1.
;-------------

    show NRO-DOC-SER-SD 
    show NRO-DOC-NRO-SD 

    fast-action
       when ENTER exit.
       WHEN FAR  perform Avanza_Numero 
                 loop.
       WHEN BAR  perform Retroc_Numero
                 loop.
       WHEN KEND MOVE ER TO LAST-KEY exit.
       when KMOD  perform MODIFI-NUMERO1
                       when ER exit.
       end-when
    loop.



ShowDocActVen.
;-------------
    show COD-VENDE-SD
    show NRO-DOCUMEN-SD
    show FECHA-EMISI-SD
    fast-action
       WHEN KEND MOVE ER TO LAST-KEY.
       when KMOD  perform MODIFI-NUMERO
                       when ER exit.
       end-when
    exit.

MODIFI-NUMERO.
;-------------
    accept NRO-DOCUMEN-SD
        when OK move nNroDoc to folio-tab-ini
                move nNroDoc to folio-tab-fol
                rewrite REG-TAB-FOL exit.
        when KEND exit.
    loop.

MODIFI-NUMERO1.
;-------------
    accept NRO-DOC-SER-SD
    accept NRO-DOC-NRO-SD
        when BAR loop.
        when OK move nNroDoc to folio-tab-ini
                move nNroDoc to folio-tab-fol
                rewrite REG-TAB-FOL exit.
        when KEND exit.
    loop.

Avanza_Numero.
;-------------
 add  1       to nNroFac
 move nNroDoc to folio-tab-fol
 rewrite REG-TAB-FOL
 exit.

Retroc_Numero.
;-------------
 subtract   1  from  nNroFac
 move nNroDoc to folio-tab-fol
 rewrite REG-TAB-FOL
 exit.



VERIFICA-STK.
;---------------
move    0   to  nStock
perform OPENREF-ARC-INV
     when ER exit.
seek FIRST in ARC-INV-F
     when EOF exit.
search FIRST for cCodPrd in ARC-INV-F
    when ER beep
            message 1, 3, 'No hay en Stock '
            move OK to last-key.
    when OK move 0 to nStock
            read ARC-INV-F
            move canti-arc-inv to nStock
            if  nStock = 0
                beep
                message 1, 3, 'No hay en Stock '
                exit.
            show MUESTRA-STOCK-SD
    end-when
exit.


;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                               OPEN ARCHIVOS                                ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
OPENREF-IDENTIF-F.
;-----------------
    move 'IDENTI'     to nombre-fn
    move nCodVd       to codigo-fn
    move ws-file-name to file-name
    open REF IDENTIFICA-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, 'Archivo Identif.'
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENREF-TAB-RUT.
;---------------
    move 'TABRUT'     to nombre-fn
    move nCodVd       to codigo-fn
    move ws-file-name to file-name
    open REF TAB-RUT-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, '   Tabla Ruta   '
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENREF-ARC-INV.
;---------------
    if lFlgIvCs = 0
       move 'INVENT'     to nombre-fn
    else
       move 'CSGINV'     to nombre-fn
    end-if
    move nCodVd       to codigo-fn
    move ws-file-name to file-name
    open REF ARC-INV-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, '   Inventario   '
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENUP-ARC-INV-F.
;----------------
    if lFlgIvCs = 0
       move 'INVENT'     to nombre-fn
    else
       move 'CSGINV'     to nombre-fn
    end-if
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE ARC-INV-F
         when ER create ARC-INV-F
                 loop.
    exit.


OPENREF-MAE-PRO-F.
;-----------------
    move 'MAE-PROD'   to file-name.
    open REF MAE-PRO-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, 'Arch.  Productos'
                 message 49, '****************'
                 wait 1 second.

    exit.

OPENUP-ARC-CAR-F.
;----------------
    move 'CARGAS'     to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE ARC-CAR-F
         when ER create ARC-CAR-F
                 loop.
    exit.

OPENREF-ARC-CAR-F.
;-----------------
    move 'CARGAS'     to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open REF ARC-CAR-F
    exit.

OPENUP-CFA-VTA-F.
;----------------
    move 'CFA-XX'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE CFA-VTA-F
         when ER create CFA-VTA-F
                 loop.
    exit.

OPENREF-CFA-VTA-F.
;-----------------
    move 'CFA-XX'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open REF CFA-VTA-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, 'Cabecera Factura'
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENUP-DET-VTA-F.
;----------------
    move 'DFA-XX'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE DFA-VTA-F
         when ER create DFA-VTA-F
                 loop.
    exit.

OPENUP-EVENTUALF.
;----------------
    move 'EVENTF'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE EVENTUALF-F
         when ER create EVENTUALF-F
                 loop.
    exit.

OPENUP-EVENTUALE.
;----------------
    move 'EVENTE'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE EVENTUALE-F
         when ER create EVENTUALE-F
                 loop.
    exit.

OPENUP-EVENTUALC.
;----------------
    move 'EVENTC'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE EVENTUALC-F
         when ER create EVENTUALC-F
                 loop.
    exit.

OPENUP-PRIN-01-F.
;----------------
    move 'PRIN01'     to file-name.
    open UPDATE PRIN-01-F
         when ER create PRIN-01-F
                 loop.
    exit.

OPENREF-PRIN-01-F.
;-----------------
    move 'PRIN01' to file-name.
    open REF PRIN-01-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, ' Arch.  Prin01  '
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENREF-PRIN-14-F.
;-----------------
    move 'PRIN14' to file-name.
    open REF PRIN-14-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, ' Arch.  Prin01  '
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENUP-PRIN-02-F.
;----------------
    move 'PRIN02'     to file-name.
    open UPDATE PRIN-02-F
         when ER create PRIN-02-F
                 loop.
    exit.

OPENREF-PRIN-02-F.
;-----------------
    move 'PRIN02' to file-name.
    open REF PRIN-02-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, ' Arch.  Prin02  '
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENUP-PRIN-04-F.
;----------------
    move 'PRIN04'     to file-name.
    open UPDATE PRIN-04-F
         when ER create PRIN-04-F
                 loop.
    exit.

OPENUP-PRIN-14-F.
;----------------
    move 'PRIN14'     to file-name.
    open UPDATE PRIN-14-F
         when ER create PRIN-14-F
                 loop.
    exit.

OPENREF-PRIN-04-F.
;-----------------
    move 'PRIN04' to file-name.
    open REF PRIN-04-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, ' Arch.  Prin04  '
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENREF-PRIN-13-F.
;-----------------
    move 'PRIN13' to file-name.
    open REF PRIN-13-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, ' Arch.  Prin04  '
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENUP-PRIN-05-F.
;----------------
    move 'PRIN05'     to file-name.
    open UPDATE PRIN-05-F
         when ER create PRIN-05-F
                 loop.
    exit.

OPENREF-PRIN-05-F.
;-----------------
    move 'PRIN05' to file-name.
    open REF PRIN-05-F
    exit.

OPENUP-PRIN-06-F.
;----------------
    move 'PRIN06'     to file-name.
    open UPDATE PRIN-06-F
         when ER create PRIN-06-F
                 loop.
    exit.

OPENUP-PRIN-08-F.
;----------------
    move 'PRIN08'     to file-name.
    open UPDATE PRIN-08-F
         when ER create PRIN-08-F
                 loop.
    exit.

OPENREF-PRIN-08-F.
;----------------
    move 'PRIN08'     to file-name.
    open ref PRIN-08-F
    exit.

OPENUP-PRIN-09-F.
;----------------
    move 'PRIN09'     to file-name.
    open UPDATE PRIN-09-F
         when ER create PRIN-09-F
                 loop.
    exit.
OPENUP-PRIN-13-F.
;----------------
    move 'PRIN13'     to file-name.
    open UPDATE PRIN-13-F
         when ER create PRIN-13-F
                 loop.
    exit.

OPENREF-PRIN-09-F.
;----------------
    move 'PRIN09'     to file-name.
    open REF PRIN-09-F
    exit.

OPENREF-PRIN-15-F.
;----------------
    move 'PRIN15'     to file-name.
    open REF PRIN-15-F
    exit.


OPENUP-PRIN-10-F.
;----------------
    move 'PRIN10'     to file-name.
    open UPDATE PRIN-10-F
         when ER create PRIN-10-F
                 loop.
    exit.

OPENREF-PRIN-10-F.
;-----------------
    move 'PRIN10'     to file-name.
    open REF PRIN-10-F
    exit.

OPENREF-PRIN-06-F.
;-----------------
    move 'PRIN06' to file-name.
    open REF PRIN-06-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, ' Arch.  Prin06  '
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENUP-PRIN-07-F.
;----------------
    move 'PRIN07'     to file-name.
    open UPDATE PRIN-07-F
         when ER create PRIN-07-F
                 loop.
    exit.

OPENUP-PRIN-15-F.
;----------------
    move 'PRIN15'     to file-name.
    open UPDATE PRIN-15-F
         when ER create PRIN-15-F
                 loop.
    exit.

OPENREF-PRIN-07-F.
;-----------------
    move 'PRIN07' to file-name.
    open REF PRIN-07-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, ' Arch.  Prin07  '
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENREF-PRIN-11-F.
;-----------------
    move 'PRIN11' to file-name.
    open REF PRIN-11-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, ' Arch.  Prin07  '
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENUP-PRIN-11-F.
;----------------
    move 'PRIN11'     to file-name.
    open UPDATE PRIN-11-F
         when ER create PRIN-11-F
                 loop.
    exit.

OPENUP-PRIN-12-F.
;----------------
    move 'PRIN12'     to file-name.
    open UPDATE PRIN-12-F
         when ER create PRIN-12-F
                 loop.
    exit.

OPENUP-PRIN-16-F.
;----------------
    move 'PRIN16'     to file-name.
    open UPDATE PRIN-16-F
         when ER create PRIN-16-F
                 loop.
    exit.

OPENUP-PRIN-17-F.
;----------------
    move 'PRIN17'     to file-name.
    open UPDATE PRIN-17-F
         when ER create PRIN-17-F
                 loop.
    exit.

OPENREF-PRIN-17-F.
;-----------------
    move 'PRIN17' to file-name.
    open REF PRIN-17-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, ' Arch.  Prin07  '
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENUP-PRIN-19-F.
;----------------
    move 'PRIN19'     to file-name.
    open UPDATE PRIN-19-F
         when ER create PRIN-19-F
                 loop.
    exit.

OPENREF-PRIN-19-F.
;-----------------
    move 'PRIN19' to file-name.
    open REF PRIN-19-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, ' Arch.  Prin19  '
                 message 49, '****************'
                 wait 1 second.
    exit.


OPENUP-PRIN-18-F.
;----------------
    move 'PRIN18'     to file-name.
    open UPDATE PRINT-18-F
         when ER create PRINT-18-F
                 loop.
    exit.

OPENREF-PRIN-18-F.
;-----------------
    move 'PRIN18' to file-name.
    open REF PRINT-18-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, ' Arch.  Prin18  '
                 message 49, '****************'
                 wait 1 second.
    exit.


OPENUP-DET-ENV-F.
;----------------
    move 'ENV-XX'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE DET-ENV-F
         when ER create DET-ENV-F
                 loop.
    exit.

OPENUP-PEDIDOS-F.
;----------------
    move 'PED-XX'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE PEDIDOS-F
         when ER create PEDIDOS-F
                 loop.
    exit.

OPENREF-PEDIDOS-F.
;-----------------
    move 'PED-XX'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open REF PEDIDOS-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, ' Arch.  Pedidos '
                 message 49, '****************'
                 wait 1 second.
    exit.


OPENUP-DET-CSG-F.
;----------------
    move 'CSGPEN'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE DET-CSG-F
         when ER create DET-CSG-F
                 loop.
    exit.

OPENREF-DET-ENV-F.
;-----------------
    move 'ENV-XX'       to nombre-fn.
    move nCodVd         to codigo-fn.
    move ws-file-name to file-name.
    open REF DET-ENV-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, 'Detalle  Envases'
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENREF-DET-CSG-F.
;-----------------
    move 'CSGPEN'       to nombre-fn.
    move nCodVd         to codigo-fn.
    move ws-file-name to file-name.
    open REF DET-CSG-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, 'Detalle  Consig.'
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENREF-MAE-CLIE.
;----------------
    move 'MAECLI'       to nombre-fn.
    move nCodVd         to codigo-fn.
    move ws-file-name to file-name.
    open REF MAE-CLI-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, 'Maestro Clientes'
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENUP-MAE-CLIE.
;----------------
    move 'MAECLI'       to nombre-fn.
    move nCodVd         to codigo-fn.
    move ws-file-name to file-name.
    open update MAE-CLI-F
         when ER create MAE-CLI-F
                 loop.
    exit.

; Archivo de Cobranzas.

OPENUP-DCO-PAG-F.
;----------------
    move 'DCO-XX'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE DCO-PAG-F
         when ER create DCO-PAG-F
                 loop.
    exit.

OPENUP-ARC-CHE-F.
;----------------
    move 'ACH-XX'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE ARC-CHE-F
         when ER create ARC-CHE-F
                 loop.
    exit.

OPENREF-ARC-CHE-F.
;-----------------
    move 'ACH-XX'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open REF ARC-CHE-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, 'Archivo  Cheques'
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENUP-ARC-NUE-F.
;----------------
    move 'ANU-XX'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE ARC-NUE-F
         when ER create ARC-NUE-F
                 loop.
    exit.

OPENREF-ARC-COB-F.
;-----------------
    move 'ACO-XX'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open REF ARC-COB-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, 'Archivo Cobranza'
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENUP-ARC-COB-F.
;-----------------
    move 'ACO-XX'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE ARC-COB-F
         when ER create  ARC-COB-F
                 loop.
    exit.

OPENUP-CAB-PRE-F.
;----------------
    move 'PRESTA'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE CAB-PRE-F
         when ER create CAB-PRE-F
                 loop.
    exit.

OPENUP-CAB-CSG-F.
;----------------
    move 'CONSIG'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE CAB-CSG-F
         when ER create CAB-CSG-F
                 loop.
    exit.

OPENUP-DEV-ENV-F.
;----------------
    move 'DEVENV'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE DEV-ENV-F
         when ER create DEV-ENV-F
                 loop.
    exit.

OPENUP-ENV-CSG-F.
;----------------
    move 'ENVCSG'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE ENV-CSG-F
         when ER create ENV-CSG-F
                 loop.
    exit.

OPENREF-ENV-CSG-F.
;-----------------
    move 'ENVCSG'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open REF ENV-CSG-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, 'Archivo EnvCsg  '
                 message 49, '****************'
                 wait 1 second.
    exit.


OPENUP-PrEvCsg-F.
;----------------
    move 'RECENV'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE PrEvCsg-F
         when ER create PrEvCsg-F
                 loop.
    exit.

OPENUP-ENV-ADE-F.
;----------------
    move 'ENVADE'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE ENV-ADE-F
         when ER create ENV-ADE-F
                 loop.
    exit.


OPENREF-ENV-ADE-F.
;-----------------
    move 'ENVADE'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open REF ENV-ADE-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, 'Archivo EnvAde  '
                 message 49, '****************'
                 wait 1 second.
    exit.


OPENUP-DEV-CSG-F.
;----------------
    move 'DEVCSG'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE DEV-CSG-F
         when ER create DEV-CSG-F
                 loop.
    exit.

OPENUP-DevCsg-F.
;----------------
    move 'DEVEVS'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE DevCsg-F
         when ER create DevCsg-F
                 loop.
    exit.

OPENREF-TAB-MOT-F.
;-----------------
    move 'TMOTIVOS'       to file-name.
    open REF TAB-MOT-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, 'Tabla de Motivos'
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENREF-TAB-FOL-F.
;-----------------
    move 'FOL-XX'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open REF TAB-FOL-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, 'Tabla de Folios '
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENUP-TAB-FOL-F.
;----------------
    move 'FOL-XX'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE TAB-FOL-F
         when ER create TAB-FOL-F
                 loop.
    exit.

OPENUP-CAB-VAR-F.
;----------------
    move 'CABVAR'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE CAB-VAR-F
         when ER create CAB-VAR-F
                 loop.
    exit.

OPENUP-DET-VAR-F.
;----------------
    move 'DETVAR'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE DET-VAR-F
         when ER create DET-VAR-F
                 loop.
    exit.

OPENUP-ARC-ATE-F.
;----------------
    move 'ATENDI'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE ARC-ATE-F
         when ER create ARC-ATE-F
                 loop.
    exit.

OPENREF-ARC-ATE-F.
;-----------------
    move 'ATENDI'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open REF ARC-ATE-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, ' Arch. Atencion '
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENREF-PROD-CENSO.
;------------------
    move 'PCENSO'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open REF PRODUC-CENSO-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No Existen   '
                 message 33, 'Productos  Censo'
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENREF-EMBOTELLADORA.
;---------------------
    move 'EMBOTE'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open REF EMBOTELLADORA-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, 'Archivo Embotell'
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENUP-C-ENVASE.
;--------------
    move 'C-ENVA'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE CENSO-F
         when ER create CENSO-F
                 loop.
    exit.

OPENUP-C-PRECIO.
;---------------
    move 'C-PREC'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE ENCUESTA-F
         when ER create ENCUESTA-F
                 loop.
    exit.

OPENUP-MERCHAND.
;--------------
    move 'MERCHA'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE MERCHAND-F
         when ER create MERCHAND-F
                 loop.
    exit.

OPENREF-ITEM-PUBLI.
;-----------------
    move 'IT-PUB'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open REF ITEM-PUBLICID-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, 'Archivo Publicid'
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENUP-PUBLICIDAD.
;-----------------
    move 'PUBLIC'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE PUBLICIDAD-F
         when ER create PUBLICIDAD-F
                 loop.
    exit.

OPENUP-RACIONES-F.
;----------------
    move 'RACION'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE RACIONES-F
         when ER create RACIONES-F
                 loop.
    exit.

OPENUP-CANJES-F.
;---------------
    move 'CANJES'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE CANJES-F
         when ER create CANJES-F
                 loop.
    exit.

OPENREF-CANJES-F.
;---------------
    move 'CANJES'       to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open REF CANJES-F
    exit.

OPENUP-ROTURAS-F.
;---------------
    move 'ROTURA'     to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE ROTURAS-F
         when ER create ROTURAS-F
                 loop.
    exit.

OPENUP-OBSEQUIOS-F.
;------------------
    move 'OBSEQU'     to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE OBSEQUIOS-F
         when ER create OBSEQUIOS-F
                 loop.
    exit.

OPENUP-GASTOS-F.
;---------------
    move 'GASTOS'     to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE GASTOS-F
         when ER create GASTOS-F
                 loop.
    exit.

OPENUP-PAGOSF.
;---------------
    move 'PAGOSF'     to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE PAGOSF-F
         when ER create PAGOSF-F
                 loop.
    exit.

OPENUP-RECARGA-F.
;---------------
    move 'RECARG'     to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE RECARGA-F
         when ER create RECARGA-F
                 loop.
    exit.

OPENUP-DESCARGA-F.
;---------------
    move 'DESCAR'     to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open UPDATE DESCARGA-F
         when ER create DESCARGA-F
                 loop.
    exit.


OPENREF-SISTEMA-F.
;----------------
    move 'SISTEMA' to file-name.
    open REF SISTEMA-F
exit.

OPENREF-RAMOS.
;-------------
    move 'TIPNEG'     to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open REF RAMOS-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, 'Archivo de Ramos'
                 message 49, '****************'
                 wait 1 second.
    exit.

OPENREF-BANCOS.
;-------------
    move 'BANCOS'     to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open REF BANCOS-F
         when ER beep
                 message 1,  '****************'
                 message 17, '   No  Existe   '
                 message 33, 'Archivo Bancos  '
                 message 49, '****************'
                 wait 1 second.
    exit.


OPENREF-MENSAJE-CLIENTE.
;-----------------------
    move 'MENSAJ'    to nombre-fn.
    move nCodVd       to codigo-fn.
    move ws-file-name to file-name.
    open REF MENSAJE-CLI-F
    exit.


LEE-TECLA-ACTION.
;---------------
   ACTION.
EXIT.

LEE-TECLA-FAST-ACTION.
;--------------------
   FAST-ACTION.
EXIT.

END-OF-PROGRAM.
