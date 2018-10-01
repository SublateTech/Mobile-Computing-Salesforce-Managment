;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ              I D E N T I F I C A T I O N     D I V I S I O N               ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
IDENTIFICATION DIVISION.
program-id.             'DEMOVTA'.
author.                 Vกctor Martกnez P.
installation.           RIMPEXCHILE.
date-written.           27/03/96.
date-compiled.          08/04/97.
remarks.
                  ;ีออออออออออออออออออออออออออออออออออออออออธ;
                  ;ณ      DEMO DE PREVENTAS/AUTOVENTA       ณ;
                  ;ณ    Sistema de PreVenta y AutoVenta     ณ;
                  ;ิออออออออออออออออออออออออออออออออออออออออพ;

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                            ENVIRONMENT DIVISION                            ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
source-computer.    PC.
object-computer.    T710.
special-names.

        KEY  KMEM    IS 1.
        KEY  NO      IS 2.
        KEY  YES     IS 3.
        KEY  ADJ     IS 4.
        KEY  KVER    IS 5.
        KEY  KNEXT   IS 6.
        KEY  KBUS    IS 7.
        KEY  KNOMB   IS 8.
        KEY  KSGTE   IS 9.
        KEY  KDEL    IS 10.
        KEY  KBEEP   IS 11.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                                DATA DIVISION                               ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
DATA DIVISION.
FILE SECTION.

                  ;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ;
                  ;ณ          ARCHIVOS DE ENTRADA          ณ;
                  ;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู;

FD CLIENTES-F                    ; Archivo de clientes
   recording mode is fixed       ; CLIENTES.E
   type is 'E'.

01 REG-CLIENTES.
   03 cli-codigo           pic X(12).      ; codigo cliente
   03 cli-rut.
      05 cli-numero        pic 9(10).      ; rut cliente
      05 cli-guion         pic X(01).
      05 cli-digito        pic X(01).
   03 cli-nombre           pic X(30).      ; nombre o razon social
   03 cli-direccion        pic X(30).      ; direccion
   03 cli-telefono         pic X(07).      ; telefono
   03 cli-ciudad           pic X(02).      ; codigo ciudad
   03 cli-zona             pic 9(02).      ; numero region
   03 cli-comuna           pic X(06).      ; comuna
   03 cli-condicion-pago   pic X(02).      ; codigo condicion de pago
   03 cli-bloqueo          pic 9(01).      ; 0=bloqueado
   03 cli-linea-credito    pic 9(08).      ; linea de credito
   03 cli-deuda-pendiente  pic 9(08).      ; deuda pendiente
   03 cli-cr               pic 9(01) binary.
   03 cli-lf               pic 9(01) binary.

FD CIUDAD-F                      ; Archivo de CIUDADES
   recording mode is fixed       ; CIUDAD.E
   type is 'E'.

01 REG-CIUDAD.
   03 ciu-codigo           pic X(02).      ; codigo ciudda
   03 ciu-nombre           pic X(30).      ; nombre ciudad
   03 ciu-cr               pic 9(01) binary.
   03 ciu-lf               pic 9(01) binary.

FD CONDICION-PAGO-F              ; Archivo de CONDICIONES DE PAGO
   recording mode is fixed       ; CONDPAGO.E
   type is 'E'.

01 REG-CONDICION-PAGO.
   03 cpa-codigo           pic X(02).      ; codigo condicion de pago
   03 cpa-nombre           pic X(30).      ; nombre condicion de pago
   03 cpa-decto-financiero pic 9(02)V99.   ; Descuento Financiero
   03 cpa-dias             pic 9(03).      ; Cantidad de dias plazo
   03 cpa-cr               pic 9(01) binary.
   03 cpa-lf               pic 9(01) binary.

FD FAMILIA-F                     ; Archivo de FAMILIAS DE PRODUCTO
   recording mode is fixed       ; FAMILIA.E
   type is 'E'.

01 REG-FAMILIA.
   03 fam-codigo           pic X(02).      ; codigo familia
   03 fam-nombre           pic X(30).      ; nombre familia
   03 fam-cr               pic 9(01) binary.
   03 fam-lf               pic 9(01) binary.

FD SUB-FAMILIA-F                 ; Archivo de SUB-FAMILIAS DE PRODUCTO
   recording mode is fixed       ; SUBFAMIL.E
   type is 'E'.

01 REG-SUB-FAMILIA.
   03 sfa-codigo.
      05 sfa-cod-familia   pic X(02).      ; codigo familia
      05 sfa-cod-subfam    pic X(02).      ; codigo sub-familia
   03 sfa-nombre           pic X(30).      ; nombre sub-familia
   03 sfa-cr               pic 9(01) binary.
   03 sfa-lf               pic 9(01) binary.

FD PRODUCTOS-F                   ; Archivo de PRODUCTOS
   recording mode is fixed       ; PRODUC<bodega>.E
   type is 'E'.

01 REG-PRODUCTOS.
   03 pro-codigo.
      05 pro-busca-familias.
         07 pro-familia     pic X(02).      ; codigo familia producto
         07 pro-sub-familia pic X(02).      ; codigo sub-familia producto
      05 pro-producto       pic X(06).      ; codigo producto
   03 pro-descripcion       pic X(30).      ; descripcion producto
   03 pro-unidad-medida     pic X(06).      ; unidad de medida
   03 pro-stock             pic S9(04)V99.  ; stock disponible
   03 pro-cr                pic 9(01) binary.
   03 pro-lf                pic 9(01) binary.

FD LISTA-PRECIO-F                ; Archivo de LISTAS DE PRECIOS
   recording mode is fixed       ; LIS<lista_de_precio>.E
   type is 'E'.

01 REG-LISTA-PRECIO.
   03 lpr-producto         pic X(10).      ; codigo producto
   03 lpr-precio           pic 9(06)V99.   ; precio producto
   03 lpr-descuento        pic 9(02)V99.   ; descuento producto (no automatico)
   03 lpr-cant-bonifica    pic 9(04)V99.   ; cantidad necesaria para bonificar
   03 lpr-bonificacion     pic 9(04)V99.   ; cantidad con que se bonifica
   03 lpr-cr               pic 9(01) binary.
   03 lpr-lf               pic 9(01) binary.

FD LISTAS-DISPONIBLES-F          ; Archivo de LISTAS DE PRECIOS DISPONIBLES
   recording mode is fixed       ; LISTAS.E
   type is 'E'.

01 REG-LISTA-DISPONIBLES.
   03 ldi-lista-precio     pic 9(05).      ; lista de precio
   03 ldi-nombre-lista     pic X(16).      ; nombre lista de precio
   03 ldi-cr               pic 9(01) binary.
   03 ldi-lf               pic 9(01) binary.

FD BANCO-F                       ; Archivo de BANCOS
   recording mode is fixed       ; BANCOS.E
   type is 'E'.

01 REG-BANCO.
   03 ban-codigo           pic 9(02).      ; codigo banco
   03 ban-nombre           pic X(16).      ; nombre banco
   03 ban-cr               pic 9(01) binary.
   03 ban-lf               pic 9(01) binary.

FD PLAZA-F                       ; Archivo de PLAZAS
   recording mode is fixed       ; PLAZAS.E
   type is 'E'.

01 REG-PLAZA.
   03 pla-codigo           pic 9(02).      ; codigo plaza
   03 pla-nombre           pic X(16).      ; nombre plaza
   03 pla-cr               pic 9(01) binary.
   03 pla-lf               pic 9(01) binary.

FD PARAMETROS-F                  ; Archivo de PARAMETROS GENERALES
   recording mode is fixed       ; PARAMETR.E
   type is 'E'.

01 REG-PARAMETROS.
   03 par-codigo-vendedor  pic X(02).      ; codigo vendedor
   03 par-nombre-vendedor  pic X(16).      ; nombre vendedor
   03 par-numero-factura   pic 9(09).      ; correlativo facturas
   03 par-numero-pedido    pic 9(04).      ; correlativo numero pedido
   03 par-km-salida        pic 9(07).      ; kilometraje de salida (autoventa)
   03 par-km-llegada       pic 9(07).      ; kilometraje de llegada (autoventa)
   03 par-fecha-salida     pic 9(06).      ; fecha de salida (dia,mes,aคo)
   03 par-fecha-llegada    pic 9(06).      ; fecha de llegada (dia,mes,aคo)
   03 par-hora-salida      pic 9(04).      ; hora de salida (hora,minuto)
   03 par-hora-llegada     pic 9(04).      ; hora de llegada (hora,minuto)
   03 par-iva              pic 9(02).      ; I.V.A.
   03 par-adicional        pic 9(02).      ; impuesto adicional
   03 par-maximo-itemes    pic 9(02).      ; cantidad maxina itemes autoventa
   03 par-pvta-autoventa   pic 9(01).      ; 0=autoventa, 1=preventa
   03 par-bodega           pic 9(02).      ; bodega por defecto
   03 par-password         pic X(05).      ; password para cambiar reloj
   03 par-log              pic 9(01).      ; 0=graba archivo log, 1=no graba
   03 par-cr               pic 9(01) binary.
   03 par-lf               pic 9(01) binary.


                   ;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ;
                   ;ณ          ARCHIVOS DE SALIDA          ณ;
                   ;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู;

FD CLIENTE-NUEVO-F               ; archivo de CLIENTES NUEVOS
   recording mode is fixed       ; CLINUEVO.S
   type is 'S'.

01 REG-CLIENTE-NUEVO.
   03 cln-codigo           pic X(12).      ; codigo cliente
   03 cln-nombre           pic X(30).      ; nombre cliente
   03 cln-direccion        pic X(30).      ; direccion cliente
   03 cln-ciudad           pic X(02).      ; codigo ciudad
   03 cln-rut              pic 9(08).      ; rut cliente
   03 cln-digito           pic X(01).      ; digito verificador del rut
   03 cln-telefono         pic X(07).      ; telefono cliente
   03 cln-cr               pic 9(01) binary.
   03 cln-lf               pic 9(01) binary.

FD CABECERA-PEDIDO-F             ; archivo de CABECERA DE PEDIDO
   recording mode is fixed       ; CABECERA.S
   type is 'S'.

01 REG-CABECERA-PEDIDO.
   03 cpe-numero-pedido    pic 9(04).      ; numero de pedido
   03 cpe-numero-factura   pic 9(09).      ; numero de factura
   03 cpe-cliente          pic X(12).      ; codigo cliente
   03 cpe-lista-precio     pic 9(05).      ; lista de precio
   03 cpe-fecha            pic 9(06).      ; fecha preventa
   03 cpe-hora             pic 9(04).      ; hora preventa
   03 cpe-bodega           pic 9(02).      ; bodega
   03 cpe-total            pic 9(08).      ; total vendido en $
   03 cpe-decto-financiero pic 9(02)V99.   ; descuento financiero
   03 cpe-forma-pago       pic X(02).      ; codigo forma de pago
   03 cpe-fecha-vencim     pic 9(06).      ; dia,mes,aคo (se calcula automat.)
   03 cpe-cr               pic 9(01) binary.
   03 cpe-lf               pic 9(01) binary.

FD DETALLE-PEDIDO-F              ; archivo de DETALLES DE PEDIDO
   recording mode is fixed       ; DETALLES.S
   type is 'S'.

01 REG-DETALLE-PEDIDO.
   03 dpe-llave.
      05 dpe-numero-pedido pic 9(04).      ; numero de pedido
      05 dpe-producto      pic X(10).      ; codigo producto
   03 dpe-cantidad         pic 9(06).      ; cantidad vendida
   03 dpe-total-item       pic 9(06)V99.   ; sub total del item
   03 dpe-descuento        pic 9(02)V99.   ; descuento al item
   03 dpe-cr               pic 9(01) binary.
   03 dpe-lf               pic 9(01) binary.

FD BONIFICACION-F                ; archivo de BONIFICACIONES
   recording mode is fixed       ; BONIFICA.S
   type is 'S'.

01 REG-BONIFICACION.
   03 bon-numero-pedido    pic 9(04).      ; numero de pedido
   03 bon-producto         pic X(10).      ; codigo producto
   03 bon-cantidad         pic 9(06).      ; cantidad bonificada
   03 bon-cr               pic 9(01) binary.
   03 bon-lf               pic 9(01) binary.

FD PAGO-VENTA-F                  ; archivo de PAGOS DE LA VENTA
   recording mode is fixed       ; PAGOSVTA.S
   type is 'S'.

01 REG-PAGO-VENTA.
   03 pon-numero-pedido    pic 9(04).      ; numero de pedido
   03 pon-documento        pic 9(10).      ; numero cheque
   03 pon-serie            pic X(10).      ; numero de serie del cheque
   03 pon-cuenta-cte       pic 9(10).      ; cuenta corriente
   03 pon-banco            pic 9(02).      ; codigo banco
   03 pon-plaza            pic 9(02).      ; codigo plaza
   03 pon-monto            pic 9(08)V99.   ; monto cancelado
   03 pon-cr               pic 9(01) binary.
   03 pon-lf               pic 9(01) binary.

FD OBSERVACIONES-F               ; archivo de OBSERVACIONES
   recording mode is fixed       ; OBSERVA.S
   type is 'S'.

01 REG-OBSERVACIONES.
   03 obs-nro-pedido       pic 9(04).      ; numero pedido
   03 obs-observacion      pic X(48).      ; observacion
   03 obs-cr               pic 9(01) binary.
   03 obs-lf               pic 9(01) binary.

FD VISITAS-F                     ; archivo de VISITAS
   recording mode is fixed       ; VISITAS.S
   type is 'S'.

01 REG-VISITAS.
   03 vis-cliente          pic X(12).      ; codigo cliente
   03 vis-observacion      pic X(48).      ; observacion
   03 vis-fecha            pic 9(04).      ; fecha
   03 vis-hora             pic 9(06).      ; hora
   03 vis-cr               pic 9(01) binary.
   03 vis-lf               pic 9(01) binary.

FD LOG-F                         ; archivo LOG
   recording mode is fixed       ; LOG.S
   type is 'S'.

01 REG-LOG.
   03 log-movimiento       pic X(05).      ; codigo movimiento
   03 log-hora             pic 9(04).      ; hora movimiento
   03 log-cr               pic 9(01) binary.
   03 log-lf               pic 9(01) binary.

                   ;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ;
                   ;ณ                VARIOS                ณ;
                   ;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู;

FD FILELIST-F
   recording mode is fixed               ; Lista de archivos a enviar
   type is 'V'.

01 HOST-PARAMS-R.                       ; Primer registro en archivo.
   03 TIPO-HOST-1        pic X(3).      ; Tipo Host:'PTC','PC ','TDS'.
   03 VELOCIDAD-1        pic 9(2).      ; Velocid:12,24,96,19
   03 TELEFONO           pic X(15).
   03 SUBMIT-FILE        pic X(8).      ; Nombre de SUBMIT en el HOST.
   03 IDENTIF-PTC        pic X(8).      ; Id. para POLLING.
   03 MODO-COMUN         pic 9.         ; 0 ASCII - 1 Transparent
   03 TIPO-COMUN         pic 9.         ; 1 Directa - 2 Modem - 3  Polling
   03 TIPO-MODEM         pic 9.         ; 0 interno, 1 externo
   03 TIPO-DISC          pic X.         ; P Pulso, T Tono, M manual
   03 I-BORRA-ARCH       pic 9.         ; 0 No Borra - 1 Borra despus de enviar
   03 ESTADO-COM         pic 9(01).     ; 0 exitosa, 1 error

FD PROGRAMA-F                       ; Archivo para Efectuar
   recording mode is FREE           ; las Llamadas a los
   type is program.                 ; Distintos Programas

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                          WORKING STORAGE SECTION                           ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
WORKING-STORAGE section.

01 W-MEMORIA.
   03 memory-flag             pic 9(02) binary.
   03 display-save            pic X(132).
   03 ws-mem-avail            pic 9(06) binary.

01 W-FECHA-HORA.
   03 hrs                     pic 9(02).
   03 ws-ampm                 pic 9(02).

01 FECHA-WS     packed.
   03 ws-ano                  pic 9(02) packed.
   03 ws-mes                  pic 9(02) packed.
   03 ws-dia                  pic 9(02) packed.
   03 ws-dow                  pic 9(02) packed.
   03 ws-hora                 pic 9(02) packed.
   03 ws-minuto               pic 9(02) packed.
   03 ws-segundo              pic 9(02) packed.
   03 ws-hora-ampm            pic 9(02) packed.
   03 ws-12-24                pic 9(02) packed.

01 W-VARIABLES1.
   03 w-fecha.
      05 w-fecha-dia          pic 9(02).
      05 w-fecha-mes          pic 9(02).
      05 w-fecha-ano          pic 9(02).
   03 w-hora.
      05 w-hora-hrs           pic 9(02).
      05 w-hora-min           pic 9(02).
   03 punt-mp                 pic 9(01) binary.
   03 punt-miscelaneo         pic 9(02) binary.
   03 w-prevta-autovta        pic X(09).
   03 w-ing-password          pic X(05).
   03 punt-fecha              pic 9(01) binary.
   03 punt-error              pic 9(02) binary.
   03 w-codigo-vendedor       pic X(02).
   03 w-nombre-vendedor       pic X(16).
   03 w-password              pic X(05).
   03 w-numero-factura        pic 9(09).
   03 w-numero-pedido         pic 9(04).
   03 w-km-salida             pic 9(07).
   03 w-km-llegada            pic 9(07).
   03 w-iva                   pic 9(02).
   03 w-iva-adicional         pic 9(02).
   03 w-maximo-itemes         pic 9(02).
   03 w-pvta-autoventa        pic 9(01).
   03 w-bodega                pic 9(02).
   03 w-revisa-bodega         pic 9(02).
   03 w-decto-financiero      pic 9(02)V99.
   03 punt-venta              pic 9(01) binary.
   03 w-existe-ciudad         pic 9(01).
   03 w-existe-condicion-pago pic 9(01).
   03 w-existe-banco          pic 9(01).
   03 w-existe-plaza          pic 9(01).
   03 w-existe-clinuevo       pic 9(01).
   03 w-cod-cliente           pic X(12).
   03 w-busca-cliente.
      05 w-busca-rut          pic 9(10).
      05 w-busca-guion        pic X(01).
      05 w-busca-digito       pic X(01).

01 W-VARIABLES2.
   03 w-nombre-cliente.
      05 w-nombre-16          pic X(16).
      05 w-nombre-14          pic X(14).
   03 w-direccion.
      05 w-dire-16            pic X(16).
      05 w-dire-14            pic X(14).
   03 w-cond-pago-scroll.
      05 w-cond-pago-16       pic X(16).
      05 w-cond-pago-14       pic X(14).
   03 w-ciudad                pic X(30).
   03 w-cond-pago             pic X(30).
   03 w-ciudad-scroll.
      05 w-ciudad-16          pic X(16).
      05 w-ciudad-14          pic X(14).
   03 w-consulta-saldo        pic 9(08).
   03 w-log                   pic 9(01).
   03 w-sino-log              pic X(02).
   03 w-lista-precio          pic 9(05).

01 W-VARIABLES3.
   03 w-familia               pic X(30).
   03 w-familia-scroll.
      05 w-familia-16         pic X(16).
      05 w-familia-14         pic X(14).
   03 w-sub-familia           pic X(30).
   03 w-sub-familia-scroll.
      05 w-sub-familia-16     pic X(16).
      05 w-sub-familia-14     pic X(14).
   03 w-precio                pic 9(06)V99.
   03 w-busca-familias.
      05 w-busca-cod-familia  pic X(02).
      05 w-busca-cod-sub-fam  pic X(02).
   03 w-busca-producto        pic X(08).
   03 w-busca-familia         pic X(02).
   03 w-busca-sub-familia     pic X(02).
   03 w-codigo-producto.
      05 w-cod-familias.
         07 w-cod-prod-familia pic X(02).
         07 w-cod-prod-sub-fam pic X(02).
      05 w-cod-prod-resto      pic X(06).
   03 w-largo-nombre           pic 9(02) binary.
   03 w-llave-detalle.
      05 w-llave1-detalle      pic 9(04).
      05 w-llave2-detalle      pic X(10).
   03 w-stock                  pic S9(04)V99.
   03 w-cantidad               pic 9(04)V99.
   03 w-mod-cantidad           pic 9(04)V99.
   03 w-descuento              pic 9(02)V99.
   03 w-mod-descuento          pic 9(02)V99.
   03 w-bonificacion           pic 9(04)V99.
   03 w-calculo1               pic 9(14) binary.
   03 w-calculo2               pic 9(14) binary.
   03 w-calculo3               pic 9(14) binary.
   03 w-total-item             pic 9(08) binary.
   03 w-total-venta            pic 9(08) binary.
   03 w-mod-bonificacion       pic 9(04)V99.
   03 w-cuenta-itemes          pic 9(02).
   03 w-fin-venta              pic 9(01).
   03 w-existe-precio          pic 9(01).
   03 w-existe-detalle         pic 9(01).
   03 w-es-modificacion        pic 9(01).
   03 w-cod-cond-pago          pic X(02).

01 W-VARIABLES4.
   03 w-rut-clinuevo.
      05 w-clinue-rut          pic 9(08).
      05 w-clinue-digito       pic X(01).
   03 w-rut-mueve.
      05 w-mueve-rut           pic 9(10).
      05 w-mueve-guion         pic X(01).
      05 w-mueve-digito        pic X(01).
   03 w-nombre-clinue          pic X(30).
   03 w-fono-clinue            pic X(07).
   03 w-direc-clinue           pic X(30).
   03 punt-corrige             pic 9(01) binary.
   03 w-cod-ciudad             pic X(02).
   03 w-cond-pago-efectivo     pic X(02).
   03 w-ultima-condicion       pic 9(01).
   03 w-credito-venta          pic S9(12).
   03 w-item-antiguo           pic 9(06).
   03 w-tot-neto               pic 9(08).
   03 w-tot-iva                pic 9(08).
   03 w-tot-bruto              pic 9(08).
   03 punt-pagos               pic 9(01) binary.
   03 w-nro-docum              pic 9(10).
   03 w-cta-cte                pic 9(10).
   03 w-monto-pago             pic 9(08).
   03 w-cod-banco              pic 9(02).
   03 w-cod-plaza              pic 9(02).
   03 w-nro-serie              pic X(10).
   03 w-nro-factura2           pic 9(09).
   03 w-tmp-rut-clinuevo.
      05 w-tmp-rut             pic 9(08).
      05 w-tmp-guion           pic X(01).
      05 w-tmp-digito          pic X(01).
   03 w-busca-ciudad           pic X(02).
   03 w-lineas-impresas        pic 9(02).
   03 w-contabiliza-lineas     pic S9(03).
   03 w-es-cliente-nuevo       pic 9(01).

01 W-VARIABLES5.
   03 w-cod-prod.
      05 w-cod-prod-16         pic X(16).
      05 w-cod-prod-filler     pic X(14).
   03 w-llave-bonificacion.
      05 w-llave-bon1          pic 9(04).
      05 w-llave-bon2          pic X(10).
   03 w-pantalla               pic 9(01).
   03 w-hay-bonificacion       pic 9(01).
   03 w-existe-lis-pre         pic 9(01).
   03 w-existe-producto        pic 9(01).
   03 w-saldo-pago             pic S9(08).
   03 w-devuelve-credito       pic 9(01).
   03 w-largo-cod-prod         pic 9(01) binary.
   03 w-observacion            pic X(48).
   03 w-compara-obs            pic X(48).
   03 w-existe-obs             pic 9(01).
   03 w-tope-bonificacion      pic 9(04)V99.
   03 w-cod-prod-tmp           pic X(06).
   03 w-tecla-beep             pic 9(01) binary.

01 W-VARABLES6.
   03 w-arreglo-vencim         pic 9(02) occurs 12 times. ; TABLA TOTAL DIAS POR MES.
   03 w-fecha-vencimiento.
      05 w-venc-dia            pic 9(02).
      05 w-venc-mes            pic 9(02).
      05 w-venc-ano            pic 9(02).
   03 w-dias-vencimiento       pic 9(03).
   03 w-itera-dias             pic 9(03).
   03 w-calculo0               pic 9(10) binary.
   03 w-indice                 pic 9(01) binary.

01 VAR-IMPRESION1-CABECERA.
   03 w-prn-nom-cliente        pic X(30).
   03 w-prn-direccion          pic X(30).
   03 w-prn-comuna             pic X(06).
   03 w-prn-ciudad             pic X(30).
   03 w-prn-zona               pic 9(20).
   03 w-prn-rut                pic X(12).
   03 w-prn-telefono           pic X(07).
   03 w-prn-forma-pago         pic X(30).
   03 w-prn-cod-vendedor       pic X(02).
   03 w-prn-nom-vendedor       pic X(16).
   03 w-prn-cliente            pic X(12).

01 VAR-IMPRESION2-CABECERA.
   03 w-prn-nro-factura        pic 9(09).
   03 w-prn-dia-vencim         pic 9(02).
   03 w-prn-mes-vencim         pic X(03).
   03 w-prn-ano-vencim         pic 9(02).
   03 w-prn-dia                pic 9(02).
   03 w-prn-mes                pic X(03).
   03 w-prn-ano                pic 9(02).
   03 w-prn-neto               pic 9(08).
   03 w-prn-decto-financ       pic 9(08).
   03 w-prn-sub-total          pic 9(08).
   03 w-prn-total-iva          pic 9(08).
   03 w-prn-total-final        pic 9(08).

01 VAR-IMPRESION-DETALLES.
   03 w-prn-cod-producto       pic X(10).
   03 w-prn-desc-producto      pic X(30).
   03 w-prn-desc-detalle       pic 9(02)V99.
   03 w-prn-cant-detalle       pic 9(04)V99.
   03 w-prn-unidad-medida      pic X(06).
   03 w-prn-deta-precio        pic 9(06)V99.
   03 w-prn-total-detalle      pic 9(06).
   03 w-agrega-item            pic 9(01).

01 W-VAR-01-PRODUCTO.
   03 w-nombre-producto.
      05 w-prod-23            pic X(23).
      05 w-prod-07            pic X(07).
   03 w-producto-01           pic X(01).
   03 w-producto-02           pic X(02).
   03 w-producto-03           pic X(03).
   03 w-producto-04           pic X(04).
   03 w-producto-05           pic X(05).
   03 w-producto-06           pic X(06).
   03 w-producto-07           pic X(07).
   03 w-producto-08           pic X(08).
   03 w-producto-09           pic X(09).
   03 w-producto-10           pic X(10).

01 W-VAR-02-PRODUCTO.
   03 w-producto-11           pic X(11).
   03 w-producto-12           pic X(12).
   03 w-producto-13           pic X(13).
   03 w-producto-14           pic X(14).
   03 w-producto-15           pic X(15).
   03 w-producto-16           pic X(16).
   03 w-producto-17           pic X(17).
   03 w-producto-18           pic X(18).
   03 w-producto-19           pic X(19).
   03 w-producto-20           pic X(20).

01 W-VAR-03-PRODUCTO.
   03 w-producto-21           pic X(21).
   03 w-producto-22           pic X(22).
   03 w-producto-23           pic X(23).
   03 w-producto-24           pic X(24).
   03 w-producto-25           pic X(25).
   03 w-producto-26           pic X(26).
   03 w-producto-27           pic X(27).
   03 w-producto-28           pic X(28).
   03 w-producto-29           pic X(29).

01 VAR-NOMBRES-ARCHIVOS.
   03 file-lis-pre.
      05 file-1-lis-pre       pic X(03).
      05 file-2-lis-pre       pic 9(05).
   03 file-producto.
      05 file-1-producto      pic X(06).
      05 file-2-producto      pic 9(02).

01 W-VAR-SCROLL-01.
   03 w-miscelaneo02.
      05 w-misc21             pic X(01).
      05 w-misc22             pic X(16).
      05 w-misc23             pic X(13).
   03 w-miscelaneo03.
      05 w-misc31             pic X(02).
      05 w-misc32             pic X(16).
      05 w-misc33             pic X(12).
   03 w-miscelaneo04.
      05 w-misc41             pic X(03).
      05 w-misc42             pic X(16).
      05 w-misc43             pic X(11).
   03 w-miscelaneo05.
      05 w-misc51             pic X(04).
      05 w-misc52             pic X(16).
      05 w-misc53             pic X(10).

01 W-VAR-SCROLL-02.
   03 w-miscelaneo06.
      05 w-misc61             pic X(05).
      05 w-misc62             pic X(16).
      05 w-misc63             pic X(09).
   03 w-miscelaneo07.
      05 w-misc71             pic X(06).
      05 w-misc72             pic X(16).
      05 w-misc73             pic X(08).
   03 w-miscelaneo08.
      05 w-misc81             pic X(07).
      05 w-misc82             pic X(16).
      05 w-misc83             pic X(07).
   03 w-miscelaneo09.
      05 w-misc91             pic X(08).
      05 w-misc92             pic X(16).
      05 w-misc93             pic X(06).
   03 w-miscelaneo10.
      05 w-misc101            pic X(09).
      05 w-misc102            pic X(16).
      05 w-misc103            pic X(05).

01 W-VAR-SCROLL-03.
   03 w-miscelaneo11.
      05 w-misc111            pic X(10).
      05 w-misc112            pic X(16).
      05 w-misc113            pic X(04).
   03 w-miscelaneo12.
      05 w-misc121            pic X(11).
      05 w-misc122            pic X(16).
      05 w-misc123            pic X(03).
   03 w-miscelaneo13.
      05 w-misc131            pic X(12).
      05 w-misc132            pic X(16).
      05 w-misc133            pic X(02).
   03 w-miscelaneo14.
      05 w-misc141            pic X(13).
      05 w-misc142            pic X(16).
      05 w-misc143            pic X(01).
   03 w-miscelaneo15.
      05 w-misc151            pic X(14).
      05 w-misc152            pic X(16).

01 RUT-1                      redefines.
   03 ws-num                  pic 9(01) occurs 10 times.

01 OTROS.
   03 rest-aux.                                    ; variables para hacer
      05 resto-aux1           pic 9(02).           ; calculo del rut
      05 resto-aux            pic 9(02) binary.
   03 ws-resto.
      05 ws-resto1            pic 9(03) binary.
   03 ws-dig                  pic X(01).
   03 ws-con                  pic 9(02) binary.
   03 ws-tbl                  pic 9(02) binary.
   03 ws-suma                 pic 9(03) binary.
   03 ws-num-aux              pic 9(02) binary.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                               CHECK SECTION                                ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
CHECK section.

ch CK-MES
   type is range
   size is  2
   occurs   1 times
   value is '01' '12' OK.

ch CK-DIA
   type is range
   size is 2
   occurs 1 times
   value is '01' '31' OK.

ch CK-HORA
   type is range
   size is 2
   occurs 1 times
   value is '00' '24' OK.

ch CK-MINUTO
   type is range
   size is 2
   occurs 1 times
   value is '00' '60' OK.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                              KEYBOARD SECTION                              ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
KEYBOARD SECTION.
kd  std-710  value is

; Unshifted

        off   DAR     KBUS  UAR    KEND
        ER    ER      ER    KDEL   KNEXT
        ER    ER      ER    LIGHT  KMEM
        KNOMB ER      KBEEP ADJ    ER
        KSGTE ER      ER    KVER   ER
      shift   YES     ER    NO     CLR
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

;SD VTO-SD.
;03 w-item-antiguo
;   clear
;   edit
;   prompt 1 1 'ItemAntig:'
;   mask  11 1 '999999'.
;03 w-total-item
;   edit
;   prompt 1 2 'TotItem:'
;   mask   9 2 '99999999'.
;03 w-total-venta
;   edit
;   prompt 1 3 'TotVent:'
;   mask   9 3 '99999999'.
;03 w-credito-venta
;   edit
;   prompt 1 4 'Cr:'
;   mask   4 4 '999999999999-'.

SD MEM-REM-S.
03 ws-mem-avail
   prompt 17 '           Bytes'
   mask   19 'ZZZZZ9'
   edit.

SD PRENDE-PTC-SD.
03 w-codigo-vendedor
   clear
   prompt 1 1 '  DEMO VENTAS    Sistema Ventas   Vendedor:'
   mask  13 3 'XX'.
03 w-nombre-vendedor
   mask 1 4 'XXXXXXXXXXXXXXXX'.

SD PARAM-01-SD.
03 w-codigo-vendedor
   prompt 1 2 ' Vendedor :     '
   mask  13 2 'XX'.
03 w-nombre-vendedor
   prompt 1 3 ' '
   mask   2 3 'XXXXXXXXXXXXXXXX'.
03 w-numero-factura
   edit
   prompt 1 4 ' #Fact:         '
   mask   8 4 'ZZZZZZZZ9'.

SD PARAM-02-SD.
03 w-numero-pedido
   edit
   prompt 1 2 ' #Pedido :      '
   mask  12 2 'ZZZ9'.
03 w-km-salida
   edit
   prompt 1 3 ' KM -->:        '
   mask  10 3 'ZZZZZZ9'.
03 w-km-llegada
   edit
   prompt 1 4 ' KM <--:        '
   mask  10 4 'ZZZZZZ9'.

SD PARAM-03-SD.
03 w-iva
   edit
   prompt 1 2 ' I.V.A. :    %  '
   mask  11 2 'Z9'.
03 w-maximo-itemes
   edit
   prompt 1 3 ' Item x Factu:  '
   mask  15 3 'Z9'.
03 w-prevta-autovta
   prompt 1 4 '                '
   mask   5 4 'XXXXXXXXX'.

SD PARAM-04-SD.
03 w-bodega
   edit
   prompt 1 2 ' Bodega :       '
   mask  11 2 'Z9'.
03 w-fecha-dia
   edit
   prompt 1 3 '   /  /     :   '
   mask   2 3 '99'.
03 w-fecha-mes
   edit
   mask 5 3 '99'.
03 w-fecha-ano
   edit
   mask 8 3 '99'.
03 w-hora-hrs
   edit
   mask 11 3 '99'.
03 w-hora-min
   edit
   prompt 1 4 '                '
   mask  14 3 '99'.
03 w-sino-log
   prompt 1 4 ' Archivo LOG:   '
   mask  15 4 'XX'.

SD PARAM-100-SD.
03 w-numero-factura
   edit
   prompt 2 4 '#Fact:'
   mask   8 4 'ZZZZZZZZ9'.

SD PARAM-120-SD.
03 w-km-salida
   edit
   prompt 2 3 'KM -->:'
   mask  10 3 'ZZZZZZ9'.

SD PARAM-130-SD.
03 w-km-llegada
   edit
   prompt 2 4 'KM <--:'
   mask  10 4 'ZZZZZZ9'.

SD PARAM-140-SD.
03 w-bodega
   edit
   prompt 2 2 'Bodega :'
   mask  11 2 'Z9'.

SD PARAM-150-SD.
03 w-ing-password
   edit
   prompt 2 4 'Password: '
   mask  12 4 'BBBBB'.

SD DIA-SD.
03 w-fecha-dia
   edit
   auto-record
   mask 2 3 '99'
   check is ck-dia
   min 2.

SD MES-SD.
03 w-fecha-mes
   edit
   auto-record
   mask 5 3 '99'
   check is ck-mes
   min 2.

SD ANO-SD.
03 w-fecha-ano
   edit
   auto-record
   mask 8 3 '99'
   min 2.

SD HORA-SD.
03 w-hora-hrs
   edit
   auto-record
   mask 11 3 '99'
   check is ck-hora
   min 2.

SD MINUTO-SD.
03 w-hora-min
   edit
   auto-record
   mask 14 3 '99'
   check is ck-minuto
   min 2.

SD CLIENTE-SD.
03 w-busca-rut
   clear
   edit
   prompt 1 1 'Rut:          - '
   mask   5 1 'ZZZZZZZZZ9'.

SD CLIENTE-DIG-SD.
03 w-busca-digito
   auto-shift
   mask 16 1 'X'.

SD SEL-CLIENTE-SD.
03 cli-codigo
   clear
   prompt 1 1 'Cod:'
   mask   5 1 'XXXXXXXXXXXX'.
03 w-nombre-16
   mask 1 2 'XXXXXXXXXXXXXXXX'.
03 w-dire-16
   mask 1 3 'XXXXXXXXXXXXXXXX'.
03 w-cond-pago-16
   mask 1 4 'XXXXXXXXXXXXXXXX'.

SD FONO-Y-CUPO-SD.
03 cli-telefono
   clear
   edit
   prompt 1 1 'Telefono:'
   mask  10 1 'ZZZZZZ9'.
03 cli-telefono
   clear
   edit
   prompt 1 1 'Telefono:'
   mask  10 1 'XXXXXXX'.
03 w-ciudad-16
   mask 1 2 'XXXXXXXXXXXXXXXX'.
03 cli-comuna
   prompt 2 3 'Comuna:'
   mask  10 3 'XXXXXX'.
03 w-consulta-saldo
   edit
   prompt 1 4 'Saldo $'
   mask   9 4 'ZZZZZZZ9'.

SD SCROLL-CLIENTE-SD.
03 w-nombre-16
   mask 1 2 'XXXXXXXXXXXXXXXX'.
03 w-dire-16
   mask 1 3 'XXXXXXXXXXXXXXXX'.
03 w-cond-pago-16
   mask 1 4 'XXXXXXXXXXXXXXXX'.

SD SCROLL-CIUDAD-SD.
03 w-ciudad-16
   mask 1 2 'XXXXXXXXXXXXXXXX'.

SD LISTA-DE-PRECIO-SD.
03 ldi-nombre-lista
   clear
   prompt 1 1 'Lista de Precio:'
   mask   1 2 'XXXXXXXXXXXXXXXX'.
03 ldi-lista-precio
   edit
   prompt 1 3 '********************       *****'
   mask   6 4 'ZZZZ9'.

SD FAMILIA-SD.
03 fam-codigo
   clear
   prompt 1 1 'Familia    :'
   mask  14 1 'XX'.
03 w-familia-16
   mask 1 2 'XXXXXXXXXXXXXXXX'.

SD SUB-FAMILIA-SD.
03 sfa-cod-subfam
   prompt 1 3 'Sub-Familia:'
   mask  14 3 'XX'.
03 w-sub-familia-16
   mask 1 4 'XXXXXXXXXXXXXXXX'.

SD SEL-PRODUCTO-SD.
03 pro-codigo
   clear
   edit
   prompt 1 1 'Cod:'
   mask   5 1 'XX-XX-XXXXXX'.
03 w-prod-23
   mask 1 2 'XXXXXXXXXXXXXXXXXXXXXXX'.
03 pro-stock
   edit
   mask 9 3 'ZZZ9,99-'.
03 w-precio
   edit
   prompt 3 4 '$'
   mask   5 4 'ZZZZZ9,99'.

SD SCROLL-PRODUCTO-SD.
03 w-nombre-producto
   auto-shift
   prompt 15 3 '  '
   mask    1 2 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

SD BUSCA-CLI-NOMBRE-SD.
03 w-nombre-producto
   clear 2 - 3
   auto-shift
   mask 1 2 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

SD UNIDAD-MEDIDA-SD.
03 pro-unidad-medida
   clear 4
   prompt 1 3 'Unidad de Medida'
   mask   6 4 'XXXXXX'.

SD CODIGO-PRODUCTO-SD.
03 w-codigo-producto
   clear
   edit
   prompt 1 1 'Cod:'
   mask   5 1 'XX-XX-XXXXXX'.

SD CANTIDAD-SD.
03 w-cantidad
   clear
   edit
   prompt 1 1 'Cantidad:'
   mask  10 1 'ZZZ9,99'.

SD FALTA-STOCK-SD.
03 w-stock
   clear 3 - 4
   edit
   prompt 1 3 'Stock Disponible'
   mask   5 4 'ZZZ9,99-'.

SD ING-BONIFICACION-SD.
03 w-bonificacion
   edit
   prompt 1 2 'Bonifica:'
   mask  10 2 'ZZZ9,99'.

SD MUE-BONIFICACION-SD.
03 w-tope-bonificacion
   edit
   prompt 1 3 '**** Maximo ****'
   mask   5 4 'ZZZ9,99'.

SD DESCUENTO-SD.
03 w-descuento
   edit
   prompt 1 3 'Descuento:     %'
   mask  11 3 'Z9,99'.

SD TOPE-DESCUENTO-SD.
03 lpr-descuento
   edit
   prompt 1 4 'Max.Descu:     %'
   mask  11 4 'Z9,99'.

SD TOPE-2DESCUENTO-SD.
03 lpr-descuento
   edit
   prompt 1 3 'Max.Descu:     %'
   mask  11 3 'Z9,99'.

SD SUB-TOTAL-SD.
03 w-total-item
   edit
   prompt 1 4 'Item $'
   mask   8 4 'ZZZZZZZ9'.

SD TOTAL-VENTA-SD.
03 w-total-venta
   edit
   prompt 1 3 'Total  Acumulado  $'
   mask   5 4 'ZZZZZZZ9'.

SD CANTIDAD2-SD.
03 w-mod-cantidad
   use-last
   edit
   prompt 1 1 'Cantidad:'
   mask  10 1 'ZZZ9,99'.

SD ING-BONIF2-SD.
03 w-mod-bonificacion
   use-last
   edit
   prompt 1 2 'Bonifica:'
   mask  10 2 'ZZZ9,99'.

SD DESCUENTO2-SD.
03 w-mod-descuento
   use-last
   edit
   prompt 1 3 'Descuento:     %'
   mask  11 3 'Z9,99'.

SD CONFIRMA-ITEM-SD.
03 pro-descripcion
   clear
   prompt 2 1 'Confirma Item?'
   mask   1 2 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

SD RECONFIRMA-ITEM-SD.
03 pro-descripcion
   clear
   prompt 2 1 'Corrige Item ?'
   mask   1 2 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

SD CLINUEVO-CODIGO-SD.
03 w-cod-cliente
   prompt 1 2 'Cod:'
   mask   5 2 'XXXXXXXXXXXX'.

SD CLINUEVO-RUT-SD.
03 w-clinue-rut
   clear
   edit
   prompt 1 1 'Rut:         -'
   mask   6 1 'ZZZZZZZ9'.

SD CLINUEVO-DIGITO-SD.
03 w-clinue-digito
   auto-shift
   mask 15 1 'X'.

SD CIUDAD-SD.
03 w-cod-ciudad
   prompt 4 3 'Ciudad:'
   mask  12 3 'XX'.
03 w-ciudad-16
   mask 1 4 'XXXXXXXXXXXXXXXX'.

SD CIUDAD-30-SD.
03 ciu-nombre
   clear 3 - 4
   mask 1 3 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

SD CLINUE-NOMBRE-SD.
03 w-nombre-clinue
   clear
   auto-shift
   prompt 1 1 'Nombre/Razon Soc'
   mask   1 2 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

SD CLINUE-FONO-SD.
03 w-fono-clinue
   prompt 1 4 'Telefono:'
   mask  10 4 'XXXXXXX'.

SD CLINUE-DIRECCION-SD.
03 w-direc-clinue
   clear
   auto-shift
   prompt 1 1 '** Direccion ***'
   mask   1 2 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

SD FORMA-PAGO-SD.
03 cpa-codigo
   clear
   prompt 1 1 '*Condicion Pago*'
   mask   8 2 'XX'.
03 cpa-nombre
   mask 1 3 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

SD NO-HAY-MAS-CREDITO.
03 w-credito-venta
   clear
   edit
   prompt 1 1 'No le queda cupo  para comprar  ** DISPONIBLE **$'
   mask   2 4 'ZZZZZZZZZZZ9+'.

SD TOTAL-FINAL-SD.
03 w-total-venta
   clear
   edit
   prompt 1 1 'Neto $'
   mask   8 1 'ZZZZZZZ9'.
03 w-tot-iva
   edit
   prompt 1 3 'IVA $'
   mask   8 3 'ZZZZZZZ9'.
03 w-tot-bruto
   edit
   prompt 1 4 'Bruto $'
   mask   8 4 'ZZZZZZZ9'.

SD DESCUENTO-FINAL-SD.
03 w-descuento
   edit
   prompt 1 2 'Descuento:     %'
   mask  11 2 'Z9,99'.

SD DESCUENTO-FINANCIERO-SD.
03 w-decto-financiero
   edit
   prompt 1 3 'Sobrepasa MaximoDescuento      %'
   mask  11 4 'Z9,99'.

SD NRO-DOCUM-SD.
03 w-nro-docum
   clear
   edit
   prompt 1 1 '#Docu:'
   mask   7 1 'ZZZZZZZZZ9'.

SD NRO-SERIE-SD.
03 w-nro-serie
   auto-shift
   prompt 1 2 'Serie:'
   mask   7 2 'XXXXXXXXXX'.

SD CTA-CTE-SD.
03 w-cta-cte
   edit
   prompt 1 3 'Cuenta Corriente'
   mask   4 4 'ZZZZZZZZZ9'.

SD MONTO-PAGO-SD.
03 w-monto-pago
   edit
   prompt 2 1 'Monto del Pago'
   mask   5 2 'ZZZZZZZ9'.

SD SALDO-PAGO-SD.
03 w-saldo-pago
   edit
   prompt 1 3 '**** Saldo *****'
   mask   5 4 'ZZZZZZZ9+'.

SD BANCO-SD.
03 ban-codigo
   clear
   edit
   prompt 1 1 'Banco :'
   mask   9 1 'Z9'.
03 ban-nombre
   mask 1 2 'XXXXXXXXXXXXXXXX'.

SD PLAZA-SD.
03 pla-codigo
   edit
   prompt 1 3 'Plaza :'
   mask   9 3 'Z9'.
03 pla-nombre
   mask 1 4 'XXXXXXXXXXXXXXXX'.

SD NRO-FACTURA-SD.
03 w-numero-factura
   clear
   edit
   prompt 2 1 'Numero Factura ****************'
   mask   4 4 'ZZZZZZZZ9'.

SD PEDIDO-A-CORREGIR-SD.
03 cpe-numero-pedido
   clear
   edit
   prompt 3 1 'Pedido:'
   mask  11 1 'ZZZ9'.
03 cpe-cliente
   prompt 1 2 'Cli:'
   mask   5 2 'XXXXXXXXXXXX'.
03 w-nombre-16
   mask 1 3 'XXXXXXXXXXXXXXXX'.
03 cpe-total
   edit
   prompt 3 4 '$'
   mask   5 4 'ZZZZZZZ9'.

SD SCROLL-CLI-REVISA-PED-SD.
03 w-nombre-cliente
   mask 1 3 'XXXXXXXXXXXXXXXX'.

SD CORRIGE-ITEM-SD.
03 dpe-producto
   clear
   prompt 1 1 'Prod.:'
   mask   7 1 'XXXXXXXXXX'.
03 w-cod-prod-16
   mask 1 2 'XXXXXXXXXXXXXXXX'.

SD CORRIGE-ITEM-SCROLL-SD.
03 w-cod-prod
   mask 1 2 'XXXXXXXXXXXXXXXX'.

SD PANTALLA-01-SD.
03 dpe-cantidad
   edit
   prompt 1 3 'Cantidad:'
   mask  10 3 'ZZZ9,99'.
03 dpe-total-item
   edit
   prompt 1 4 'Item $'
   mask   8 4 'ZZZZZ9'.

SD PANTALLA-02-SD.
03 w-bonificacion
   edit
   prompt 1 3 'Bonifica:'
   mask  10 3 'ZZZ9,99'.
03 dpe-descuento
   edit
   prompt 1 4 'Descuento:     %'
   mask  11 4 'Z9,99'.

SD MODI-CANTIDAD-SD.
03 w-cantidad
   edit
   prompt 1 3 'Cantidad:'
   mask  10 3 'ZZZ9,99'.

SD MODI-DESCUENTO-SD.
03 w-descuento
   edit
   prompt 1 4 'Descuento:     %'
   mask  11 4 'Z9,99'.

SD MODI-BONIFICACION-SD.
03 w-bonificacion
   edit
   prompt 1 3 'Bonifica:'
   mask  10 3 'ZZZ9,99'.

SD REVISA-PAGOS-SD.
03 pon-documento
   clear
   edit
   prompt 1 1 'Docum:'
   mask   7 1 'ZZZZZZZZZ9'.
03 pon-serie
   prompt 1 2 'Serie:'
   mask   8 2 'XXXXXX'.
03 pon-cuenta-cte
   edit
   prompt 1 3 'Cuent:'
   mask   7 3 'ZZZZZZZZZ9'.
03 pon-monto
   edit
   prompt 1 4 'Monto $'
   mask   8 4 'ZZZZZZZ9'.

SD OBSERVACION-SD.
03 w-observacion
   clear
   auto-shift
   prompt 1 1 '  Observacion   '
   mask   1 3 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                  SECCION DE COMUNICACIONES ( IMPRESION )                   ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
COMMUNICATION section.

CD PRINTER-COM-80                      ; Impresora ZFP-80 TELXON (de 80 cols..)
   type is        BURST
   baud-rate is   9600
   data-bits      8
   parity         NONE
   mode is        ASCII
   block contains 40 bytes.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                            SECCION DE REPORTES                             ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
REPORT section.

RD REP-CABECERA
   nlit x'1b0f'
   nlit x'0d0d'
   lit '             '
   edit w-prn-nom-cliente 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
   nlit x'0d'
   lit '             '
   edit w-prn-direccion 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
   nlit x'0d'
   lit '             '
   edit w-prn-comuna 'XXXXXX'
   lit '  '
   edit w-prn-ciudad 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
   nlit x'0d'
   lit '             '
   edit w-prn-rut 'XXXXXXXXXXXX'
   nlit x'0d0d'
   lit '             '
   edit w-prn-zona 'XXXXXXXXXXXXXXXXXXXX'
   lit '          '
   lit '          '
   lit '          '
   lit '          '
   lit '          '
   edit w-prn-cod-vendedor 'XX'
   lit ' '
   edit w-prn-nom-vendedor 'XXXXXXXXXXXXXXXX'
   nlit x'0d'
   lit '             '
   edit w-prn-telefono 'XXXXXXX'
   nlit x'0d0d'
   lit '             '
   edit w-prn-forma-pago 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
   nlit x'0d0d'
   lit '  '
   edit w-prn-cliente 'XXXXXXXXXXXX'
   lit '               '
   edit w-prn-dia-vencim '99'
   lit '-'
   edit w-prn-mes-vencim 'XXX'
   lit '-'
   edit w-prn-ano-vencim '99'
   lit '          '
   lit '          '
   lit '          '
   lit '          '
   lit '          '
   lit '          '
   lit '        '
   edit w-prn-nro-factura '999999999'
   lit '        '
   edit w-prn-dia '99'
   lit '-'
   edit w-prn-mes 'XXX'
   lit '-'
   edit w-prn-ano '99'
   nlit x'0d'.

RD REP-DETALLE
   lit '  '
   edit w-prn-cod-producto 'XXXXXXXXXX'
   lit '          '
   edit w-prn-desc-producto 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
   lit '                 '
   edit w-prn-unidad-medida 'XXXXXX'
   lit '    '
   edit w-prn-cant-detalle 'Z.ZZ9,99'
   lit '       '
   edit w-prn-desc-detalle 'Z9,99'
   lit '    '
   edit w-prn-deta-precio 'ZZZ.ZZ9,99'
   lit '         '
   edit w-prn-total-detalle 'ZZZ.ZZ9'
   nlit x'0d'.

RD REP-BONIFICACION
   lit '  '
   edit w-prn-cod-producto 'XXXXXXXXXX'
   lit '          '
   edit w-prn-desc-producto 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
   lit '          '
   lit '       '
   edit w-prn-unidad-medida 'XXXXXX'
   lit '    '
   edit w-prn-cant-detalle 'Z.ZZ9,99'
   lit '            '
   lit '          0,01               0'
   nlit x'0d'.

RD REP-TOTALES
   lit '  '
   edit w-prn-neto 'ZZ.ZZZ.ZZ9'
   lit '           '
   edit w-prn-decto-financ 'ZZ.ZZZ.ZZ9'
   nlit x'0d0d0d'
   lit '  '
   edit w-prn-sub-total 'ZZ.ZZZ.ZZ9'
   lit '          '
   lit '          '
   lit '          '
   lit '          '
   lit '          '
   lit '          '
   lit '          '
   lit '           '
   edit w-prn-total-iva 'ZZ.ZZZ.ZZ9'
   lit '          '
   lit '       '
   edit w-prn-total-final 'ZZ.ZZZ.ZZ9'
   nlit x'0d'.

RD REP-LINEA
   nlit x'0d'.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                              PROCEDURE DIVISION                            ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
PROCEDURE DIVISION.
DECLARATIVES.
DEC-1.
    when OFF   POWER-OFF
               show PRENDE-PTC-SD
               wait 7 tenths.
    when TMOUT alarm
               power-off
               show PRENDE-PTC-SD
               wait 7 tenths.
    when KMEM  perform CHECK-MEMORY
               wait 1 second.
    when ADJ   adjust-lcd UP.
    when LIGHT backlight-on 25 seconds.
    when KVER  message 1 1 '****************'
               message 1 2 '  Version  1.6  '
               message 1 3 '8 de Abril, 1996'
               message 1 4 '****************'
               wait 7 tenths.
    when KBEEP perform AJUSTA-BEEP.
end declaratives.

INITIAL SECTION.
INIT-SYSTEM.
;-----------
    perform LEE-INITIAL-SECTION.
    perform MENU-PRINCIPAL.
    loop.

LEE-INITIAL-SECTION.
;-------------------
    set-keyboard STD-710.
    SET-DECLARATIVES DEC-1.
    set-display-len 16
    move 600 to user-tmout.
    move 5   to fast-speed.
    move 15  to scroll-speed.
    move '@' to shift-char.
    key-click is 1.

    read-clock.                                  ; inicio hora en 24 hrs
    move SY-DATE-TIME to fecha-ws.
    move ws-hora      to hrs.
    move ws-hora-ampm to ws-ampm.
    if ws-ampm = 1 and hrs < 12
       add 12 to hrs.
    move hrs          to ws-hora.
    move 1            to ws-12-24.
    move fecha-ws     to SY-DATE-TIME.
    set-clock.
    move 1 to punt-mp.
    move 3 to w-tecla-beep.
    perform LEE-PARAMETROS.
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                                CHECK-MEMORY                                ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
CHECK-MEMORY.
;-------------
    move display-buffer to display-save.
    move mem-avail      to ws-mem-avail.
    clear.
    if 2000 < mem-avail
       move 2000 to mem-protect
       message 1 1, 'MEM. DISPONIBLE '
       move OK to memory-flag
       show MEM-REM-S
    else
       if 200 < mem-avail
          move 200 to mem-protect
          message 1 1, ' FALTA  MEMORIA '
          move OK to memory-flag
          show MEM-REM-S
          beep
       else
          move 200 to mem-protect
          message 1 1, ' MEMORIA  LLENA '
          move ER to memory-flag
          show MEM-REM-S.
    end-if
    wait 7 tenths.
    move display-save to display-buffer.
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                           AJUSTE DEL SONIDO BEEP                           ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
AJUSTA-BEEP.
;-----------
    examine w-tecla-beep
         when 0 volume is quiet  move 3 to w-tecla-beep.
         when 1 volume is soft   move 0 to w-tecla-beep.
         when 2 volume is medium move 1 to w-tecla-beep.
         when 3 volume is loud   move 2 to w-tecla-beep.
    exit.

LEE-PARAMETROS.
;--------------
    fill w-codigo-vendedor with ' '.
    fill w-nombre-vendedor with ' '.
    fill w-password        with ' '.
    move 0 to w-numero-factura.
    move 0 to w-numero-pedido.
    move 0 to w-km-salida.
    move 0 to w-km-llegada.
    move 0 to w-iva.
    move 0 to w-iva-adicional.
    move 0 to w-maximo-itemes.
    move 0 to w-pvta-autoventa.
    move 0 to w-bodega.
    move 0 to w-log.

    perform ABRE-REF-PARAMETROS
       when ER exit.
    seek FIRST in PARAMETROS-F
       when EOF move ER to last-key
                exit.
    read PARAMETROS-F.
    move par-codigo-vendedor  to w-codigo-vendedor.
    move par-nombre-vendedor  to w-nombre-vendedor.
    move par-password         to w-password.
    move par-numero-factura   to w-numero-factura.
    move par-numero-pedido    to w-numero-pedido.
    move par-km-salida        to w-km-salida.
    move par-km-llegada       to w-km-llegada.
    move par-iva              to w-iva.
    move par-adicional        to w-iva-adicional.
    move par-maximo-itemes    to w-maximo-itemes.
    move par-pvta-autoventa   to w-pvta-autoventa.
    move par-bodega           to w-bodega.
    move par-log              to w-log.
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                                MENU PRINCIPAL                              ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
MENU-PRINCIPAL.
;--------------
    message 1 1 'MENU PRINCIPAL  '.
    message 1 2 '1 Ventas        '.
    message 1 3 '2 Comunicaciones'.
    message 1 4 '3 Parametros    '.

    examine punt-mp
       when 0 move 3 to punt-mp loop.
       when 1 message 2 2, '>'.
       when 2 message 2 3, '>'.
       when 3 message 2 4, '>'.
       when 4 move 1 to punt-mp loop.
    fast-action
       when ENTER perform MENU-PROCESO-000.
       when DIG   if data-key > 0 and data-key < 4
                     move data-key to punt-mp
                     perform MENU-PROCESO-000
                  end-if
            end-when
       when FAR   add      1 to   punt-mp.
       when BAR   subtract 1 from punt-mp.
       when KEND  exit.
    loop.

MENU-PROCESO-000.
;----------------
    move 1 to punt-miscelaneo.
    move 1 to punt-venta.
    examine punt-mp
       when 1 perform SELECCIONA-VENTA.
       when 2 perform COMUNICACIONES.
       when 3 perform REVISA-PARAMETROS.
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                  REVISA Y/O MODIFICA PARAMETROS GENERALES                  ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
REVISA-PARAMETROS.
;-----------------
    perform LEE-PARAMETROS
       when ER exit.
    message 1 1 '** PARAMETROS **'.

    perform PARAM-01000.

    if punt-miscelaneo < 4
       show PARAM-01-SD
    else
       if punt-miscelaneo > 3 and punt-miscelaneo < 7
          show PARAM-02-SD
       else
          if punt-miscelaneo > 6 and punt-miscelaneo < 10
             show PARAM-03-SD
          else
             show PARAM-04-SD
          end-if
       end-if
    end-if

    examine punt-miscelaneo
       when 0  move 12 to punt-miscelaneo loop.
       when 1  message 1 2, '>'.             ; codigo vendedor
       when 2  message 1 3, '>'.             ; nombre vendedor
       when 3  message 1 4, '>'.             ; #factura
       when 4  message 1 2, '>'.             ; #pedido
       when 5  message 1 3, '>'.             ; km salida
       when 6  message 1 4, '>'.             ; km llegada
       when 7  message 1 2, '>'.             ; iva
       when 8  message 1 3, '>'.             ; max itemes
       when 9  message 1 4, '>'.             ; autovta/prevta
       when 10 message 1 2, '>'.             ; bodega
       when 11 message 1 3, '>'.             ; reloj
       when 12 message 1 4, '>'.             ; archivo log
       when 13 move 1 to punt-miscelaneo loop.
    fast-action
       when ENTER perform PARAM-01100.
       when FAR   add      1 to   punt-miscelaneo.
       when BAR   subtract 1 from punt-miscelaneo.
       when KEND  exit.
    loop.

PARAM-01000.
;-----------
    if par-pvta-autoventa = 0
       move 'AutoVenta' to w-prevta-autovta
    else
       move 'Pre-Venta' to w-prevta-autovta.

    if par-log = 0
       move 'SI' to w-sino-log
    else
       move 'NO' to w-sino-log.

    perform LEE-RELOJ.
    exit.

LEE-RELOJ.
;---------
    read-clock.
    move SY-DATE-TIME to fecha-ws.
    move ws-dia       to w-fecha-dia.
    move ws-mes       to w-fecha-mes.
    move ws-ano       to w-fecha-ano.
    move ws-hora      to w-hora-hrs.
    move ws-minuto    to w-hora-min.

    move 31           to w-arreglo-vencim(01).      ;  Carga Tabla Fecha
    move 29           to w-arreglo-vencim(02).
    move 31           to w-arreglo-vencim(03).
    move 30           to w-arreglo-vencim(04).
    move 31           to w-arreglo-vencim(05).
    move 30           to w-arreglo-vencim(06).
    move 31           to w-arreglo-vencim(07).
    move 31           to w-arreglo-vencim(08).
    move 30           to w-arreglo-vencim(09).
    move 31           to w-arreglo-vencim(10).
    move 30           to w-arreglo-vencim(11).
    move 31           to w-arreglo-vencim(12).
    exit.

PARAM-01100.
;-----------
    examine punt-miscelaneo
       when 3  perform PARAM-01200.               ; #factura
       when 5  perform PARAM-01400.               ; km salida
       when 6  perform PARAM-01500.               ; km llegada
       when 9  if w-pvta-autoventa = 0          ; autoventa/preventa
                  move 1 to par-pvta-autoventa
               else
                  move 0 to par-pvta-autoventa.
               rewrite REG-PARAMETROS
               perform GRABA-LOG.
       when 10 perform PARAM-01600.               ; bodega
       when 11 perform PARAM-01700.               ; cambia reloj
       when 12 perform PARAM-01700.               ; cambia log(si o no)
    beep.
    exit.

PARAM-01200.           ; #factura
;-----------
    move 0 to w-numero-factura.
    show PARAM-01-SD
    accept PARAM-100-SD
       when KEND exit.
       when OK   if entry-size = 0 exit.
                 move w-numero-factura to par-numero-factura
                 rewrite REG-PARAMETROS
                 perform GRABA-LOG
                 exit.
    loop.

PARAM-01400.           ; km salida
;-----------
    move 0 to w-km-salida.
    show PARAM-02-SD
    accept PARAM-120-SD
       when KEND exit.
       when OK   if entry-size = 0 exit.
                 move w-km-salida to par-km-salida
                 rewrite REG-PARAMETROS
                 perform GRABA-LOG
                 exit.
    loop.

PARAM-01500.           ; km llegada
;-----------
    move 0 to w-km-llegada
    show PARAM-02-SD
    accept PARAM-130-SD
       when KEND exit.
       when OK   if entry-size = 0 exit.
                 move w-km-llegada to par-km-llegada
                 rewrite REG-PARAMETROS
                 perform GRABA-LOG
                 exit.
    loop.

PARAM-01600.           ; bodega
;-----------
    move 0 to w-bodega
    show PARAM-04-SD
    accept PARAM-140-SD
       when KEND exit.
       when OK   if entry-size = 0 exit.
                 move w-bodega to par-bodega
                 rewrite REG-PARAMETROS
                 perform GRABA-LOG
                 exit.
    loop.

PARAM-01700.           ; password
;-----------
    move 1 to punt-fecha.
    fill w-ing-password with ' '.
    show PARAM-04-SD
    accept PARAM-150-SD
       when KEND exit.
       when OK   if w-ing-password = w-password
                    if punt-miscelaneo = 12      ; archivo log
                       if w-log = 0              ; si graba log
                          move 1 to par-log
                       else
                          move 0 to par-log.     ; no graba log
                       rewrite REG-PARAMETROS
                       perform GRABA-LOG
                       exit
                    end-if
                    next.                        ; reloj
                 beep
                 exit.
    loop.

PARAM-01710.           ; reloj
;-----------
    examine punt-fecha
        when 0 move 1 to punt-fecha loop.
        when 1 show PARAM-04-SD
               accept DIA-SD.                    ; Acepta dia
        when 2 show PARAM-04-SD
               show DIA-SD
               accept MES-SD.                    ; Acepta mes
        when 3 show PARAM-04-SD
               show DIA-SD
               show MES-SD
               accept ANO-SD.                    ; Acepta aคo
        when 4 perform VALIDA-FECHA.
        when 5 show PARAM-04-SD
               show DIA-SD
               show MES-SD
               show ANO-SD
               accept HORA-SD.                   ; Acepta hora.
        when 6 show PARAM-04-SD
               show DIA-SD
               show MES-SD
               show ANO-SD
               show HORA-SD
               accept MINUTO-SD.                 ; Acepta minutos.
        when 7 move w-fecha-dia to ws-dia
               move w-fecha-mes to ws-mes
               move w-fecha-ano to ws-ano
               move w-hora-hrs  to ws-hora
               move w-hora-min  to ws-minuto
               move fecha-ws    to SY-DATE-TIME
               set-clock
               perform GRABA-LOG
               exit.
    no-op
        when OK   add 1 to punt-fecha.
        when BAR  subtract 1 from punt-fecha
                  if punt-fecha = 4
                     subtract 1 from punt-fecha.
             end-if
        when KEND exit.
    loop.

VALIDA-FECHA.
;------------
    move OK to last-key.
    if w-fecha-mes = 4 or w-fecha-mes = 6 or w-fecha-mes = 9 or w-fecha-mes = 11
       if w-fecha-dia > 30
          move 1 to punt-fecha
          move 2 to punt-error
          perform MUESTRA-ERROR
          exit
       end-if
    end-if
    if w-fecha-mes = 2
       if w-fecha-dia > 29
          move 5 to punt-fecha
          move 2 to punt-error
          perform MUESTRA-ERROR
          exit
       end-if
    end-if
    if w-fecha-ano > 89 and w-fecha-ano <= 99
       no-op ; OK
    else
       if w-fecha-ano >= 0 and w-fecha-ano < 15
          no-op ; OK
       else
          move 1 to punt-fecha
          move 2 to punt-error
          perform MUESTRA-ERROR
          exit
       end-if
    end-if
    exit.

GRABA-LOG.
;---------
    if par-log = 1 and punt-miscelaneo not = 12
       exit.                                     ; no graba log
    perform LEE-RELOJ.
    perform ABRE-UP-LOG
       when MEM exit.

    examine punt-miscelaneo
       when 3  move 'FACTU' to log-movimiento.    ; #factura
       when 5  move 'KMSAL' to log-movimiento.    ; km salida
       when 6  move 'KMLLE' to log-movimiento.    ; km llegada
       when 9  move 'PV-AV' to log-movimiento.    ; autoventa/preventa
       when 10 move 'BODEG' to log-movimiento.    ; bodega
       when 11 move 'RELOJ' to log-movimiento.    ; cambia reloj
       when 12 move 'LOG  ' to log-movimiento.    ; cambia log (si o no)
    move w-hora to log-hora.
    move x'0d'  to log-cr.
    move x'0a'  to log-lf.
    write REG-LOG
       when MEM perform MEMORIA-LLENA.
    exit.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                              SELECCIONA VENTA                              ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
SELECCIONA-VENTA.
;----------------
    message 1 1 'SELECCIONA VENTA'.
    message 1 2 '1 Ingresa Venta '.
    message 1 3 '2 Revisa Venta  '.
    message 1 4 '3 Cliente Nuevo '.

    examine punt-venta
       when 0 move 3 to punt-venta loop.
       when 1 message 2 2, '>'.
       when 2 message 2 3, '>'.
       when 3 message 2 4, '>'.
       when 4 move 1 to punt-venta loop.
    fast-action
       when ENTER perform SELECC-VENTA-01000.
       when DIG   if data-key > 0 and data-key < 4
                     move data-key to punt-venta
                     perform SELECC-VENTA-01000
                  end-if
            end-when
       when FAR   add      1 to   punt-venta.
       when BAR   subtract 1 from punt-venta.
       when KEND  exit.
    loop.

SELECC-VENTA-01000.
;------------------
    perform LEE-PARAMETROS.
    examine punt-venta
       when 1 perform INGRESA-VENTA.
       when 2 perform CORRECCION.
       when 3 perform CLIENTE-NUEVO.
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                               INGRESA VENTA                                ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
INGRESA-VENTA.
;-------------
    move 0 to w-existe-ciudad.
    move 0 to w-existe-condicion-pago.
    move 0 to w-existe-banco.
    move 0 to w-existe-plaza.
    move 0 to w-cond-pago-efectivo.
    move 0 to w-dias-vencimiento.

    perform ABRE-REF-CIUDAD
       when ER move 1 to w-existe-ciudad.
    seek FIRST in CIUDAD-F
       when EOF move 1 to w-existe-ciudad.

    perform ABRE-REF-CONDICION-DE-PAGO
       when ER move 1 to w-existe-condicion-pago.
    seek FIRST in CONDICION-PAGO-F
       when EOF move 1 to w-existe-condicion-pago.
       when OK  read CONDICION-PAGO-F
                move cpa-codigo to w-cond-pago-efectivo
                move cpa-dias   to w-dias-vencimiento.

    perform ABRE-REF-BANCO
       when ER move 1 to w-existe-banco.
    seek FIRST in BANCO-F
       when EOF move 1 to w-existe-banco.

    perform ABRE-REF-PLAZA
       when ER move 1 to w-existe-plaza.
    seek FIRST in PLAZA-F
       when EOF move 1 to w-existe-plaza.

    perform ABRE-REF-CLIENTES
       when ER exit.
    seek FIRST in CLIENTES-F
       when EOF move 6 to punt-error
                perform MUESTRA-ERROR
                exit.
    read CLIENTES-F.
    move 0 to w-largo-nombre.

INGRESA-VENTA-1000.
;------------------
    move cli-codigo         to w-cod-cliente.
    move cli-nombre         to w-nombre-cliente.
    move cli-direccion      to w-direccion.
    move cli-condicion-pago to w-cod-cond-pago.
    move 1                  to w-ultima-condicion.
    move 0                  to w-devuelve-credito.
    move 0                  to w-dias-vencimiento.
    fill w-cond-pago        with '*'.
    fill w-cond-pago-scroll with '*'.
    if w-existe-condicion-pago = 0
       search FIRST for cli-condicion-pago in CONDICION-PAGO-F
          when OK read CONDICION-PAGO-F
                  move cpa-nombre to w-cond-pago
                  move cpa-nombre to w-cond-pago-scroll
                  move cpa-dias   to w-dias-vencimiento.
    end-if
    show SEL-CLIENTE-SD.
    fast-action
       when ENTER if cli-bloqueo = 0
                     move 15 to punt-error
                     perform MUESTRA-ERROR
                     loop.
                  move 0 to w-largo-nombre
                  perform VENTA.
       when FAR   perform SIGUIENTE-CLIENTE
                  move 0 to w-largo-nombre.
       when BAR   perform ANTERIOR-CLIENTE
                  move 0 to w-largo-nombre.
       when KNEXT perform MUESTRA-TELEFONO-Y-CUPO
                  move 0 to w-largo-nombre.
       when KNOMB move 0 to w-largo-nombre
                  perform PIDE-NOMBRE-CLIENTE.
       when KSGTE perform PIDE-SGTE-CLIENTE.
       when KBUS  perform BUSCA-CLI-POR-RUT
                  move 0 to w-largo-nombre.
       when CLR   if w-existe-condicion-pago = 0
                     move 0 to w-largo-nombre
                     perform CAMBIA-FORMA-PAGO
                        when KEND loop.
                     move cpa-codigo to w-cod-cond-pago
                     move cpa-dias   to w-dias-vencimiento
                     perform VENTA.
            end-when
       when UAR   perform SCROLL-CLIENTE.
       when KEND  exit.
    loop.

SIGUIENTE-CLIENTE.
;-----------------
    seek NEXT in CLIENTES-F
       when EOF beep
                seek LAST in CLIENTES-F.
    read CLIENTES-F.
    exit.

ANTERIOR-CLIENTE.
;----------------
    seek PRIOR in CLIENTES-F
       when EOF beep
                seek FIRST in CLIENTES-F.
    read CLIENTES-F.
    exit.

MUESTRA-TELEFONO-Y-CUPO.
;-----------------------
    fill w-ciudad        with '*'.
    fill w-ciudad-scroll with '*'.
    if w-existe-ciudad = 0
       search FIRST for cli-ciudad in CIUDAD-F
          when OK read CIUDAD-F
                  move ciu-nombre to w-ciudad
                  move ciu-nombre to w-ciudad-scroll.
    end-if
    move     cli-linea-credito   to   w-consulta-saldo.   ; credito disponible
    subtract cli-deuda-pendiente from w-consulta-saldo.
    show FONO-Y-CUPO-SD.
    fast-action
       when UAR   perform SCROLL-CIUDAD.
       when FAR   perform SIGUIENTE-CLIENTE.
       when BAR   perform ANTERIOR-CLIENTE.
       when ENTER exit.
       when KEND  exit.
       when KNEXT exit.
    loop.

BUSCA-CLI-POR-RUT.
;-----------------
    move 0 to w-busca-rut.
    move '-' to w-busca-guion.
    accept CLIENTE-SD
       when KEND exit.
       when OK   if entry-size = 0 loop.
                 perform BUSCA-CLI-RUT-0100
                    when KEND loop.
                 exit.
    loop.

BUSCA-CLI-RUT-0100.
;------------------
    move ' ' to w-busca-digito.
    accept CLIENTE-DIG-SD
       when KEND exit.
       when OK   if entry-size = 0 loop.
                 perform BUSCA-CLI-RUT-0200
                    when ER move KEND to last-key
                            exit.
                 search FIRST for w-busca-cliente in CLIENTES-F OFFSET IS cli-rut
                    when OK read CLIENTES-F
                            exit.
                 search FIRST for w-cod-cliente in CLIENTES-F
                 read CLIENTES-F
                 move 9 to punt-error
                 perform MUESTRA-ERROR
                 exit.
    loop.

BUSCA-CLI-RUT-0200.
;------------------
    move w-busca-rut to rut-1.
    move 2           to ws-con.
    move 10          to ws-tbl.
    move 0           to ws-suma.
    move 0           to resto-aux1.
    perform VALIDA-DIGV11.
    if w-busca-digito = ws-dig
       move OK to last-key
       exit.
    move 18 to punt-error.
    perform MUESTRA-ERROR.
    exit.

CAMBIA-FORMA-PAGO.
;-----------------
    show FORMA-PAGO-SD.
    fast-action
       when ENTER exit.
       when KEND  exit.
       when BAR   seek PRIOR in CONDICION-PAGO-F
                     when EOF beep
                              seek FIRST in CONDICION-PAGO-F.
                  read CONDICION-PAGO-F
                  move 0 to w-ultima-condicion.
       when FAR   if w-ultima-condicion = 1 or w-cod-cond-pago = w-cond-pago-efectivo
                     move 1 to w-ultima-condicion
                     beep
                     loop.
                  seek NEXT in CONDICION-PAGO-F
                     when EOF beep
                              seek LAST in CONDICION-PAGO-F.
                  read CONDICION-PAGO-F
                  if cpa-codigo = w-cod-cond-pago
                     move 1 to w-ultima-condicion.
            end-when
    loop.

PIDE-NOMBRE-CLIENTE.
;-------------------
    move 0 to w-largo-nombre.
    accept BUSCA-CLI-NOMBRE-SD
       when OK   move entry-size to w-largo-nombre
                 if w-largo-nombre = 0 beep, loop.
                 perform PIDE-CLIENT-0100
                 perform PIDE-CLIENT-0500
                 exit.
       when KEND exit.
    loop.

PIDE-CLIENT-0100.
;----------------
    message 1 4 'Espere ...      '.
    move w-nombre-producto to w-producto-01.
    move w-nombre-producto to w-producto-02.
    move w-nombre-producto to w-producto-03.
    move w-nombre-producto to w-producto-04.
    move w-nombre-producto to w-producto-05.
    move w-nombre-producto to w-producto-06.
    move w-nombre-producto to w-producto-07.
    move w-nombre-producto to w-producto-08.
    move w-nombre-producto to w-producto-09.
    move w-nombre-producto to w-producto-10.
    move w-nombre-producto to w-producto-11.
    move w-nombre-producto to w-producto-12.
    move w-nombre-producto to w-producto-13.
    move w-nombre-producto to w-producto-14.
    move w-nombre-producto to w-producto-15.
    move w-nombre-producto to w-producto-16.
    move w-nombre-producto to w-producto-17.
    move w-nombre-producto to w-producto-18.
    move w-nombre-producto to w-producto-19.
    move w-nombre-producto to w-producto-20.
    move w-nombre-producto to w-producto-21.
    move w-nombre-producto to w-producto-22.
    move w-nombre-producto to w-producto-23.
    move w-nombre-producto to w-producto-24.
    move w-nombre-producto to w-producto-25.
    move w-nombre-producto to w-producto-26.
    move w-nombre-producto to w-producto-27.
    move w-nombre-producto to w-producto-28.
    move w-nombre-producto to w-producto-29.

PIDE-CLIENT-0200.
;----------------
    if w-largo-nombre > 9 next.
    examine w-largo-nombre
       when 1 search FIRST for w-producto-01 in CLIENTES-F OFFSET IS cli-nombre.
       when 2 search FIRST for w-producto-02 in CLIENTES-F OFFSET IS cli-nombre.
       when 3 search FIRST for w-producto-03 in CLIENTES-F OFFSET IS cli-nombre.
       when 4 search FIRST for w-producto-04 in CLIENTES-F OFFSET IS cli-nombre.
       when 5 search FIRST for w-producto-05 in CLIENTES-F OFFSET IS cli-nombre.
       when 6 search FIRST for w-producto-06 in CLIENTES-F OFFSET IS cli-nombre.
       when 7 search FIRST for w-producto-07 in CLIENTES-F OFFSET IS cli-nombre.
       when 8 search FIRST for w-producto-08 in CLIENTES-F OFFSET IS cli-nombre.
       when 9 search FIRST for w-producto-09 in CLIENTES-F OFFSET IS cli-nombre.
    no-op
       when OK read CLIENTES-F.
    exit.

PIDE-CLIENT-0300.
;----------------
    if w-largo-nombre > 19 next.
    examine w-largo-nombre
       when 10 search FIRST for w-producto-10 in CLIENTES-F OFFSET IS cli-nombre.
       when 11 search FIRST for w-producto-11 in CLIENTES-F OFFSET IS cli-nombre.
       when 12 search FIRST for w-producto-12 in CLIENTES-F OFFSET IS cli-nombre.
       when 13 search FIRST for w-producto-13 in CLIENTES-F OFFSET IS cli-nombre.
       when 14 search FIRST for w-producto-14 in CLIENTES-F OFFSET IS cli-nombre.
       when 15 search FIRST for w-producto-15 in CLIENTES-F OFFSET IS cli-nombre.
       when 16 search FIRST for w-producto-16 in CLIENTES-F OFFSET IS cli-nombre.
       when 17 search FIRST for w-producto-17 in CLIENTES-F OFFSET IS cli-nombre.
       when 18 search FIRST for w-producto-18 in CLIENTES-F OFFSET IS cli-nombre.
       when 19 search FIRST for w-producto-19 in CLIENTES-F OFFSET IS cli-nombre.
    no-op
       when OK read CLIENTES-F.
    exit.

PIDE-CLIENT-0400.
;----------------
    examine w-largo-nombre
       when 20 search FIRST for w-producto-20     in CLIENTES-F OFFSET IS cli-nombre.
       when 21 search FIRST for w-producto-21     in CLIENTES-F OFFSET IS cli-nombre.
       when 22 search FIRST for w-producto-22     in CLIENTES-F OFFSET IS cli-nombre.
       when 23 search FIRST for w-producto-23     in CLIENTES-F OFFSET IS cli-nombre.
       when 24 search FIRST for w-producto-24     in CLIENTES-F OFFSET IS cli-nombre.
       when 25 search FIRST for w-producto-25     in CLIENTES-F OFFSET IS cli-nombre.
       when 26 search FIRST for w-producto-26     in CLIENTES-F OFFSET IS cli-nombre.
       when 27 search FIRST for w-producto-27     in CLIENTES-F OFFSET IS cli-nombre.
       when 28 search FIRST for w-producto-28     in CLIENTES-F OFFSET IS cli-nombre.
       when 29 search FIRST for w-producto-29     in CLIENTES-F OFFSET IS cli-nombre.
       when 30 search FIRST for w-nombre-producto in CLIENTES-F OFFSET IS cli-nombre.
    no-op
       when OK read CLIENTES-F.
    exit.

PIDE-CLIENT-0500.
;----------------
    no-op                      ; No Encontro cliente ingresado
       when ER search FIRST for w-busca-producto in CLIENTES-F
               read CLIENTES-F
               move 9 to punt-error
               perform MUESTRA-ERROR.
    exit.

PIDE-SGTE-CLIENTE.
;-----------------
    if w-largo-nombre = 0
       beep
       exit.
    perform PIDE-SGTE-CLI-0100.
    perform PIDE-CLIENT-0500.
    exit.

PIDE-SGTE-CLI-0100.
;------------------
    message 1 4 'Espere ...      '.
    move w-nombre-producto to w-producto-01.
    move w-nombre-producto to w-producto-02.
    move w-nombre-producto to w-producto-03.
    move w-nombre-producto to w-producto-04.
    move w-nombre-producto to w-producto-05.
    move w-nombre-producto to w-producto-06.
    move w-nombre-producto to w-producto-07.
    move w-nombre-producto to w-producto-08.
    move w-nombre-producto to w-producto-09.
    move w-nombre-producto to w-producto-10.
    move w-nombre-producto to w-producto-11.
    move w-nombre-producto to w-producto-12.
    move w-nombre-producto to w-producto-13.
    move w-nombre-producto to w-producto-14.
    move w-nombre-producto to w-producto-15.
    move w-nombre-producto to w-producto-16.
    move w-nombre-producto to w-producto-17.
    move w-nombre-producto to w-producto-18.
    move w-nombre-producto to w-producto-19.
    move w-nombre-producto to w-producto-20.
    move w-nombre-producto to w-producto-21.
    move w-nombre-producto to w-producto-22.
    move w-nombre-producto to w-producto-23.
    move w-nombre-producto to w-producto-24.
    move w-nombre-producto to w-producto-25.
    move w-nombre-producto to w-producto-26.
    move w-nombre-producto to w-producto-27.
    move w-nombre-producto to w-producto-28.
    move w-nombre-producto to w-producto-29.

PIDE-SGTE-CLI-0200.
;------------------
    if w-largo-nombre > 9 next.
    examine w-largo-nombre
       when 1 search NEXT for w-producto-01 in CLIENTES-F OFFSET IS cli-nombre.
       when 2 search NEXT for w-producto-02 in CLIENTES-F OFFSET IS cli-nombre.
       when 3 search NEXT for w-producto-03 in CLIENTES-F OFFSET IS cli-nombre.
       when 4 search NEXT for w-producto-04 in CLIENTES-F OFFSET IS cli-nombre.
       when 5 search NEXT for w-producto-05 in CLIENTES-F OFFSET IS cli-nombre.
       when 6 search NEXT for w-producto-06 in CLIENTES-F OFFSET IS cli-nombre.
       when 7 search NEXT for w-producto-07 in CLIENTES-F OFFSET IS cli-nombre.
       when 8 search NEXT for w-producto-08 in CLIENTES-F OFFSET IS cli-nombre.
       when 9 search NEXT for w-producto-09 in CLIENTES-F OFFSET IS cli-nombre.
    no-op
       when OK read CLIENTES-F.
    exit.

PIDE-SGTE-CLI-0300.
;------------------
    if w-largo-nombre > 19 next.
    examine w-largo-nombre
       when 10 search NEXT for w-producto-10 in CLIENTES-F OFFSET IS cli-nombre.
       when 11 search NEXT for w-producto-11 in CLIENTES-F OFFSET IS cli-nombre.
       when 12 search NEXT for w-producto-12 in CLIENTES-F OFFSET IS cli-nombre.
       when 13 search NEXT for w-producto-13 in CLIENTES-F OFFSET IS cli-nombre.
       when 14 search NEXT for w-producto-14 in CLIENTES-F OFFSET IS cli-nombre.
       when 15 search NEXT for w-producto-15 in CLIENTES-F OFFSET IS cli-nombre.
       when 16 search NEXT for w-producto-16 in CLIENTES-F OFFSET IS cli-nombre.
       when 17 search NEXT for w-producto-17 in CLIENTES-F OFFSET IS cli-nombre.
       when 18 search NEXT for w-producto-18 in CLIENTES-F OFFSET IS cli-nombre.
       when 19 search NEXT for w-producto-19 in CLIENTES-F OFFSET IS cli-nombre.
    no-op
       when OK read CLIENTES-F.
    exit.

PIDE-SGTE-CLI-0400.
;------------------
    examine w-largo-nombre
       when 20 search NEXT for w-producto-20     in CLIENTES-F OFFSET IS cli-nombre.
       when 21 search NEXT for w-producto-21     in CLIENTES-F OFFSET IS cli-nombre.
       when 22 search NEXT for w-producto-22     in CLIENTES-F OFFSET IS cli-nombre.
       when 23 search NEXT for w-producto-23     in CLIENTES-F OFFSET IS cli-nombre.
       when 24 search NEXT for w-producto-24     in CLIENTES-F OFFSET IS cli-nombre.
       when 25 search NEXT for w-producto-25     in CLIENTES-F OFFSET IS cli-nombre.
       when 26 search NEXT for w-producto-26     in CLIENTES-F OFFSET IS cli-nombre.
       when 27 search NEXT for w-producto-27     in CLIENTES-F OFFSET IS cli-nombre.
       when 28 search NEXT for w-producto-28     in CLIENTES-F OFFSET IS cli-nombre.
       when 29 search NEXT for w-producto-29     in CLIENTES-F OFFSET IS cli-nombre.
       when 30 search NEXT for w-nombre-producto in CLIENTES-F OFFSET IS cli-nombre.
    no-op
       when OK read CLIENTES-F.
    exit.

SCROLL-CLIENTE.
;--------------
    move cli-nombre    to w-miscelaneo02.
    move w-misc22      to w-nombre-16.
    move cli-direccion to w-miscelaneo02.
    move w-misc22      to w-dire-16.
    move w-cond-pago   to w-miscelaneo02.
    move w-misc22      to w-cond-pago-16.
    show SCROLL-CLIENTE-SD.
    wait 2 tenths.
    ;
    move cli-nombre    to w-miscelaneo03.
    move w-misc32      to w-nombre-16.
    move cli-direccion to w-miscelaneo03.
    move w-misc32      to w-dire-16.
    move w-cond-pago   to w-miscelaneo03.
    move w-misc32      to w-cond-pago-16.
    show SCROLL-CLIENTE-SD.
    wait 2 tenths.
    ;
    move cli-nombre    to w-miscelaneo04.
    move w-misc42      to w-nombre-16.
    move cli-direccion to w-miscelaneo04.
    move w-misc42      to w-dire-16.
    move w-cond-pago   to w-miscelaneo04.
    move w-misc42      to w-cond-pago-16.
    show SCROLL-CLIENTE-SD.
    wait 2 tenths.
    ;
    move cli-nombre    to w-miscelaneo05.
    move w-misc52      to w-nombre-16.
    move cli-direccion to w-miscelaneo05.
    move w-misc52      to w-dire-16.
    move w-cond-pago   to w-miscelaneo05.
    move w-misc52      to w-cond-pago-16.
    show SCROLL-CLIENTE-SD.
    wait 2 tenths.

SCROLL-CLI-0100.
;---------------
    move cli-nombre    to w-miscelaneo06.
    move w-misc62      to w-nombre-16.
    move cli-direccion to w-miscelaneo06.
    move w-misc62      to w-dire-16.
    move w-cond-pago   to w-miscelaneo06.
    move w-misc62      to w-cond-pago-16.
    show SCROLL-CLIENTE-SD.
    wait 2 tenths.
    ;
    move cli-nombre    to w-miscelaneo07.
    move w-misc72      to w-nombre-16.
    move cli-direccion to w-miscelaneo07.
    move w-misc72      to w-dire-16.
    move w-cond-pago   to w-miscelaneo07.
    move w-misc72      to w-cond-pago-16.
    show SCROLL-CLIENTE-SD.
    wait 2 tenths.
    ;
    move cli-nombre    to w-miscelaneo08.
    move w-misc82      to w-nombre-16.
    move cli-direccion to w-miscelaneo08.
    move w-misc82      to w-dire-16.
    move w-cond-pago   to w-miscelaneo08.
    move w-misc82      to w-cond-pago-16.
    show SCROLL-CLIENTE-SD.
    wait 2 tenths.
    ;
    move cli-nombre    to w-miscelaneo09.
    move w-misc92      to w-nombre-16.
    move cli-direccion to w-miscelaneo09.
    move w-misc92      to w-dire-16.
    move w-cond-pago   to w-miscelaneo09.
    move w-misc92      to w-cond-pago-16.
    show SCROLL-CLIENTE-SD.
    wait 2 tenths.

SCROLL-CLI-0200.
;---------------
    move cli-nombre    to w-miscelaneo10.
    move w-misc102     to w-nombre-16.
    move cli-direccion to w-miscelaneo10.
    move w-misc102     to w-dire-16.
    move w-cond-pago   to w-miscelaneo10.
    move w-misc102     to w-cond-pago-16.
    show SCROLL-CLIENTE-SD.
    wait 2 tenths.
    ;
    move cli-nombre    to w-miscelaneo11.
    move w-misc112     to w-nombre-16.
    move cli-direccion to w-miscelaneo11.
    move w-misc112     to w-dire-16.
    move w-cond-pago   to w-miscelaneo11.
    move w-misc112     to w-cond-pago-16.
    show SCROLL-CLIENTE-SD.
    wait 2 tenths.
    ;
    move cli-nombre    to w-miscelaneo12.
    move w-misc122     to w-nombre-16.
    move cli-direccion to w-miscelaneo12.
    move w-misc122     to w-dire-16.
    move w-cond-pago   to w-miscelaneo12.
    move w-misc122     to w-cond-pago-16.
    show SCROLL-CLIENTE-SD.
    wait 2 tenths.
    ;
    move cli-nombre    to w-miscelaneo13.
    move w-misc132     to w-nombre-16.
    move cli-direccion to w-miscelaneo13.
    move w-misc132     to w-dire-16.
    move w-cond-pago   to w-miscelaneo13.
    move w-misc132     to w-cond-pago-16.
    show SCROLL-CLIENTE-SD.
    wait 2 tenths.

SCROLL-CLI-0300.
;---------------
    move cli-nombre    to w-miscelaneo14.
    move w-misc142     to w-nombre-16.
    move cli-direccion to w-miscelaneo14.
    move w-misc142     to w-dire-16.
    move w-cond-pago   to w-miscelaneo14.
    move w-misc142     to w-cond-pago-16.
    show SCROLL-CLIENTE-SD.
    wait 2 tenths.
    ;
    move cli-nombre    to w-miscelaneo15.
    move w-misc152     to w-nombre-16.
    move cli-direccion to w-miscelaneo15.
    move w-misc152     to w-dire-16.
    move w-cond-pago   to w-miscelaneo15.
    move w-misc152     to w-cond-pago-16.
    show SCROLL-CLIENTE-SD.
    wait 7 tenths.
    exit.

SCROLL-CIUDAD.
;-------------
    move w-ciudad to w-miscelaneo02.
    move w-misc22 to w-ciudad-16.
    show SCROLL-CIUDAD-SD.
    wait 2 tenths.
    ;
    move w-ciudad to w-miscelaneo03.
    move w-misc32 to w-ciudad-16.
    show SCROLL-CIUDAD-SD.
    wait 2 tenths.
    ;
    move w-ciudad to w-miscelaneo04.
    move w-misc42 to w-ciudad-16.
    show SCROLL-CIUDAD-SD.
    wait 2 tenths.
    ;
    move w-ciudad to w-miscelaneo05.
    move w-misc52 to w-ciudad-16.
    show SCROLL-CIUDAD-SD.
    wait 2 tenths.

SCROLL-CIU-0100.
;---------------
    move w-ciudad to w-miscelaneo06.
    move w-misc62 to w-ciudad-16.
    show SCROLL-CIUDAD-SD.
    wait 2 tenths.
    ;
    move w-ciudad to w-miscelaneo07.
    move w-misc72 to w-ciudad-16.
    show SCROLL-CIUDAD-SD.
    wait 2 tenths.
    ;
    move w-ciudad to w-miscelaneo08.
    move w-misc82 to w-ciudad-16.
    show SCROLL-CIUDAD-SD.
    wait 2 tenths.
    ;
    move w-ciudad to w-miscelaneo09.
    move w-misc92 to w-ciudad-16.
    show SCROLL-CIUDAD-SD.
    wait 2 tenths.

SCROLL-CIU-0200.
;---------------
    move w-ciudad  to w-miscelaneo10.
    move w-misc102 to w-ciudad-16.
    show SCROLL-CIUDAD-SD.
    wait 2 tenths.
    ;
    move w-ciudad  to w-miscelaneo11.
    move w-misc112 to w-ciudad-16.
    show SCROLL-CIUDAD-SD.
    wait 2 tenths.
    ;
    move w-ciudad  to w-miscelaneo12.
    move w-misc122 to w-ciudad-16.
    show SCROLL-CIUDAD-SD.
    wait 2 tenths.
    ;
    move w-ciudad  to w-miscelaneo13.
    move w-misc132 to w-ciudad-16.
    show SCROLL-CIUDAD-SD.
    wait 2 tenths.

SCROLL-CIU-0300.
;---------------
    move w-ciudad  to w-miscelaneo14.
    move w-misc142 to w-ciudad-16.
    show SCROLL-CIUDAD-SD.
    wait 2 tenths.
    ;
    move w-ciudad  to w-miscelaneo15.
    move w-misc152 to w-ciudad-16.
    show SCROLL-CIUDAD-SD.
    wait 7 tenths.
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                               CLIENTE NUEVO                                ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
CLIENTE-NUEVO.
;-------------
    fill w-cod-cond-pago with ' '.
    move 0 to w-cond-pago-efectivo.
    move 0 to w-devuelve-credito.
    perform ABRE-REF-CONDICION-DE-PAGO
       when ER move 19 to punt-error
               perform MUESTRA-ERROR
               exit.
    seek FIRST in CONDICION-PAGO-F
       when EOF move 19 to punt-error
                perform MUESTRA-ERROR
                exit.
    read CONDICION-PAGO-F.
    move cpa-codigo to w-cond-pago-efectivo.
    move cpa-codigo to w-cod-cond-pago.
    move cpa-dias   to w-dias-vencimiento.    ; por defecto la condicion de
                                              ; pago es efectivo conta/entrega
    move 0 to w-cod-cliente.                  ; Es decir, el 1er reg. del arch.
    perform CLIENTE-NUEVO-01000.
       when KEND exit.
    perform VENTA.
       when KEND exit.
    loop.

CLIENTE-NUEVO-01000.
;-------------------
    no-op.

CLIENTE-NUEVO-02000.
;-------------------
    move 0 to w-clinue-rut.
    accept CLINUEVO-RUT-SD
       when OK   perform CLIENTE-NUEVO-03000
                    when KEND loop.
                    when BAR  loop.
                 exit.
       when KEND exit.
    loop.

CLIENTE-NUEVO-03000.
;-------------------
    fill w-clinue-digito with ' '.
    show CLINUEVO-RUT-SD.
    accept CLINUEVO-DIGITO-SD
       when OK   perform CLIENTE-NUEVO-04000
                    when ER move KEND to last-key
                            exit.
                 perform CLIENTE-NUEVO-05000
                    when BAR  loop.
                 exit.
       when KEND exit.
       when BAR  exit.
    loop.

CLIENTE-NUEVO-04000.
;-------------------
    move w-clinue-rut to rut-1.
    move 2            to ws-con.
    move 8            to ws-tbl.
    move 0            to ws-suma.
    move 0            to resto-aux1.
    perform VALIDA-DIGV11.
    if w-clinue-digito = ws-dig
       move OK to last-key
       exit.
    move 18 to punt-error.
    perform MUESTRA-ERROR.
    exit.

VALIDA-DIGV11.
;-------------
    if ws-tbl < 1 then
       move ws-suma to ws-resto1
       perform COMPARA
       exit.
    move     ws-num(ws-tbl) to ws-num-aux.
    multiply ws-num-aux     by ws-con.
    add      ws-num-aux     to ws-suma.
    if ws-con = 7
       move 1 to ws-con.
    subtract 1 from ws-tbl.
    add      1 to   ws-con.
    loop.

COMPARA.
;-------
    divide   11 into ws-resto1 remainder resto-aux1.
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

CLIENTE-NUEVO-05000.
;-------------------
    move w-clinue-rut    to w-mueve-rut.
    move '-'             to w-mueve-guion.
    move w-clinue-digito to w-mueve-digito.
    move w-rut-mueve     to w-cod-cliente.

    perform ABRE-REF-2CLIENTES
       when OK search FIRST for w-rut-mueve in CLIENTES-F OFFSET IS cli-rut
                  when OK read CLIENTES-F
                          move cli-condicion-pago to w-cod-cond-pago.
            end-when

    perform ABRE-REF-CLTE-NUEVO                ; si ya existe, revisa sus datos
       when OK search FIRST for w-rut-clinuevo in CLIENTE-NUEVO-F OFFSET IS cln-rut
                  when OK read CLIENTE-NUEVO-F
                          perform CLIENTE-NVO-10000
                          exit.
            end-when

    fill w-cod-ciudad with ' '.
    perform ABRE-REF-CIUDAD
       when ER move OK to last-key
               exit.
    seek FIRST in CIUDAD-F
       when EOF move OK to last-key
                exit.
    read CIUDAD-F.

CLIEN-NUEVO-05100.
;-----------------
    move ciu-codigo to w-cod-ciudad.
    move ciu-nombre to w-ciudad-scroll.
    show CLINUEVO-RUT-SD.
    show CLINUEVO-DIGITO-SD.
    show CLINUEVO-CODIGO-SD.
    show CIUDAD-SD.
    fast-action
       when ENTER perform CLIENTE-NUEVO-06000
                     when KEND loop.
                     when BAR  loop.
                  exit.
       when KEND  exit.
       when UAR   show CIUDAD-30-SD
                  wait 1 second.
       when FAR   seek NEXT in CIUDAD-F
                     when EOF beep
                              seek LAST in CIUDAD-F.
                  read CIUDAD-F.
       when BAR   seek PRIOR in CIUDAD-F
                     when EOF beep
                              seek FIRST in CIUDAD-F.
                  read CIUDAD-F.
    loop.

CLIENTE-NUEVO-06000.
;-------------------
    fill w-nombre-clinue with ' '.
    accept CLINUE-NOMBRE-SD
       when OK   perform CLIENTE-NUEVO-07000
                    when KEND loop.
                    when BAR  loop.
                 exit.
       when KEND exit.
    loop.

CLIENTE-NUEVO-07000.
;-------------------
    fill w-fono-clinue with ' '.
    show CLINUE-NOMBRE-SD.
    accept CLINUE-FONO-SD
       when OK   perform CLIENTE-NUEVO-08000
                    when KEND loop.
                    when BAR  loop.
                 exit.
       when KEND exit.
    loop.

CLIENTE-NUEVO-08000.
;-------------------
    fill w-direc-clinue with ' '.
    accept CLINUE-DIRECCION-SD
       when OK   perform CLIENTE-NUEVO-09000
                    when KEND loop.
                    when BAR  loop.
                 exit.
       when KEND exit.
    loop.

CLIENTE-NUEVO-09000.
;-------------------
    beep.
    show CLINUE-DIRECCION-SD.
    message 1 4 '  OK? - Yes/No  '.
    fast-action
       when ENTER perform GRABA-CLIENTE-NUEVO exit.
       when KEND  exit.
       when NO    move KEND to last-key exit.
    loop.

GRABA-CLIENTE-NUEVO.
;-------------------
    perform ABRE-UP-CLTE-NUEVO
       when MEM exit.
    move w-cod-cliente   to cln-codigo.
    move w-nombre-clinue to cln-nombre.
    move w-direc-clinue  to cln-direccion.
    move w-cod-ciudad    to cln-ciudad.
    move w-clinue-rut    to cln-rut.
    move w-clinue-digito to cln-digito.
    move w-fono-clinue   to cln-telefono.
    move x'0d'           to cln-cr.
    move x'0a'           to cln-lf.
    write REG-CLIENTE-NUEVO
       when MEM perform MEMORIA-LLENA.
    exit.

CLIENTE-NVO-10000.             ; revisa datos de cliente nuevo que ya existe
;-----------------
    fill w-ciudad-scroll with ' '.
    perform ABRE-REF-CIUDAD
       when OK search FIRST for cln-ciudad in CIUDAD-F
                  when OK read CIUDAD-F
                          move ciu-nombre to w-ciudad-scroll.
            end-when

    move cln-codigo to w-cod-cliente.
    move cln-rut    to w-clinue-rut.
    move cln-digito to w-clinue-digito.
    move cln-ciudad to w-cod-ciudad.
    show CLINUEVO-CODIGO-SD.
    show CLINUEVO-RUT-SD.
    show CLINUEVO-DIGITO-SD.
    show CIUDAD-SD.
    fast-action
       when ENTER perform CLIENTE-NVO-10100
                     when KEND loop.
                     when BAR  loop.
                  exit.
       when KEND  exit.
       when CLR   perform MODI-CLINUE-001.
    loop.

CLIENTE-NVO-10100.
;-----------------
    move cln-nombre   to w-nombre-clinue.
    move cln-telefono to w-fono-clinue.
    show CLINUE-NOMBRE-SD.
    show CLINUE-FONO-SD.
    fast-action
       when ENTER perform CLIENTE-NVO-10200
                     when KEND loop.
                     when BAR  loop.
                  exit.
       when KEND  exit.
       when CLR   perform MODI-CLINUE-001.
    loop.

CLIENTE-NVO-10200.
;-----------------
    move cln-direccion to w-direc-clinue.
    show CLINUE-DIRECCION-SD.
    fast-action
       when ENTER exit.
       when KEND   exit.
       when CLR   perform MODI-CLINUE-001.
    loop.

MODI-CLINUE-001.
;---------------
    move 1 to punt-corrige.

MODI-CLINUE-002.
;---------------
    if punt-corrige > 0 and punt-corrige < 4
       message 1 1 'CLIENTE NUEVO   '
       message 1 2 '1 Cambia Ciudad '
       message 1 3 '2 Cambia Nombre '
       message 1 4 '3 Cambia Telefon'
    else
       message 1 1 'CLIENTE NUEVO   '
       message 1 2 '4 Cambia Direcci'
       message 1 3 '                '
       message 1 4 '                '
    end-if

    examine punt-corrige
       when 0 move 4 to punt-corrige loop.
       when 1 message 2 2, '>'.
       when 2 message 2 3, '>'.
       when 3 message 2 4, '>'.
       when 4 message 2 2, '>'.
       when 5 move 1 to punt-corrige loop.
    fast-action
       when ENTER perform MODI-CLINUE-003 exit.
       when DIG   if data-key > 0 and data-key < 5
                     move data-key to punt-corrige
                     perform MODI-CLINUE-003
                     exit
                  end-if
            end-when
       when FAR   add      1 to   punt-corrige.
       when BAR   subtract 1 from punt-corrige.
       when KEND  exit.
    loop.

MODI-CLINUE-003.
;---------------
    examine punt-corrige
       when 1 perform MODI-CLINUE-005.      ; corrige ciudad
       when 2 perform MODI-CLINUE-006.      ; corrige nombre
       when 3 perform MODI-CLINUE-007.      ; corrige telefono
       when 4 perform MODI-CLINUE-008.      ; corrige direccion
    exit.

MODI-CLINUE-005.
;---------------
    perform ABRE-REF-CIUDAD
       when ER exit.
    search FIRST for cln-ciudad in CIUDAD-F
       when ER exit.
    read CIUDAD-F.

MODI-CLINUE-0051.
;----------------
    clear
    move ciu-codigo to w-cod-ciudad.
    move ciu-nombre to w-ciudad-scroll.
    show CIUDAD-SD.
    fast-action
       when ENTER move w-cod-ciudad to cln-ciudad
                  rewrite REG-CLIENTE-NUEVO
                  exit.
       when KEND  exit.
       when UAR   show CIUDAD-30-SD
                  wait 1 second.
       when FAR   seek NEXT in CIUDAD-F
                     when EOF beep
                              seek LAST in CIUDAD-F.
                  read CIUDAD-F.
       when BAR   seek PRIOR in CIUDAD-F
                     when EOF beep
                              seek FIRST in CIUDAD-F.
                  read CIUDAD-F.
    loop.

MODI-CLINUE-006.
;---------------
    clear.
    fill w-nombre-clinue with ' '.
    accept CLINUE-NOMBRE-SD
       when OK   move w-nombre-clinue to cln-nombre
                 rewrite REG-CLIENTE-NUEVO
                 exit.
       when KEND exit.
    loop.

MODI-CLINUE-007.
;---------------
    clear.
    fill w-fono-clinue with ' '.
    accept CLINUE-FONO-SD
       when OK   move w-fono-clinue to cln-telefono
                 rewrite REG-CLIENTE-NUEVO
                 exit.
       when KEND exit.
    loop.

MODI-CLINUE-008.
;---------------
    fill w-direc-clinue with ' '.
    accept CLINUE-DIRECCION-SD
       when OK   move w-direc-clinue to cln-direccion
                 rewrite REG-CLIENTE-NUEVO
                 exit.
       when KEND exit.
    loop.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                                   VENTA                                    ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
VENTA.
;-----
    perform PREGUNTA-COMPRA
       when ER   exit.
       when KEND exit.
    move 0 to w-credito-venta.
    if w-cod-cond-pago not = w-cond-pago-efectivo
       move cli-linea-credito to w-credito-venta
       multiply w-credito-venta by 105      ; 1.05 (para sumarle 5% de holgura)
       divide 100 into w-credito-venta
       subtract cli-deuda-pendiente from w-credito-venta

       if w-credito-venta <= 0 and w-pvta-autoventa not = 0    ;no es autoventa
          move 21 to punt-error
          perform MUESTRA-ERROR.
    end-if

    move 0 to w-cuenta-itemes.
    move 0 to w-fin-venta.
    move 0 to w-total-venta.
    perform ABRE-REF-LIS-DISP
       when ER exit.
    seek FIRST in LISTAS-DISPONIBLES-F
       when EOF move 8 to punt-error
                perform MUESTRA-ERROR
                exit.
    read LISTAS-DISPONIBLES-F.
    perform SEL-LISTA-DE-PRECIO.
    exit.

PREGUNTA-COMPRA.
;---------------
    message 1 1 '****************'.
    message 1 2 ' Realiza  Venta '.
    message 1 3 '    Yes / No    '.
    message 1 4 '****************'.
    fast-action
       when ENTER exit.
       when NO    next.
       when KEND  exit.
    loop.

GRABA-NO-COMPRA.
;---------------
    perform INGRESA-OBSERVACION
       when KEND exit.
    perform ABRE-UP-VISITAS
       when ER exit.
    perform LEE-RELOJ.
    move cli-codigo    to vis-cliente.
    move w-observacion to vis-observacion.
    move w-fecha       to vis-fecha.
    move w-hora        to vis-hora.
    move x'0d'         to vis-cr.
    move x'0a'         to vis-lf.
    write REG-VISITAS
       when MEM perform MEMORIA-LLENA.
    beep.
    move ER to last-key.
    exit.

INGRESA-OBSERVACION.
;-------------------
    fill w-observacion with ' '.
    accept OBSERVACION-SD
       when KEND exit.
       when OK   exit.
    loop.

SEL-LISTA-DE-PRECIO.
;-------------------
    move ldi-lista-precio to w-lista-precio.
    show LISTA-DE-PRECIO-SD.
    fast-action
       when ENTER perform ABRE-REF-LISTA-PRECIO
                     when OK seek FIRST in LISTA-PRECIO-F
                                when EOF move 11 to punt-error
                                         perform MUESTRA-ERROR
                                         loop.
                             perform VENTA-01000
                             exit.
            end-when
       when FAR   seek NEXT in LISTAS-DISPONIBLES-F
                     when EOF beep
                              seek LAST in LISTAS-DISPONIBLES-F.
                  read LISTAS-DISPONIBLES-F.
       when BAR   seek PRIOR in LISTAS-DISPONIBLES-F
                     when EOF beep
                              seek FIRST in LISTAS-DISPONIBLES-F.
                  read LISTAS-DISPONIBLES-F.
       when KEND  exit.
    loop.

VENTA-01000.
;-----------
    perform ABRE-REF-FAMILIA
       when ER exit.
    seek FIRST in FAMILIA-F
       when EOF move 4 to punt-error
                perform MUESTRA-ERROR
                exit.
    read FAMILIA-F.

SEL-FAMILIA.
;-----------
    move fam-codigo to w-busca-sub-familia.
    move fam-nombre to w-familia.
    move fam-nombre to w-familia-scroll.
    show FAMILIA-SD.
    fast-action
       when ENTER perform VENTA-02000.
       when FAR   seek NEXT in FAMILIA-F
                     when EOF beep
                              seek LAST in FAMILIA-F.
                  read FAMILIA-F.
       when BAR   seek PRIOR in FAMILIA-F
                     when EOF beep
                              seek FIRST in FAMILIA-F.
                  read FAMILIA-F.
       when UAR   perform SCROLL-FAMILIA.
       when KEND  if punt-venta = 2 exit.           ; correccion pedido
                  if w-fin-venta = 1
                     perform SEGURO-FIN-VENTA
                        when ER loop.
                     move 0 to w-descuento
                     perform FIN-VENTA.
                  exit.
    loop.

; fin de la venta
FIN-VENTA.
;---------
    move 1 to w-ultima-condicion.
    move 0 to w-decto-financiero.
    move 0 to w-descuento.
    if w-existe-condicion-pago = 0              ; existe archivo forma de pago
       search FIRST for w-cod-cond-pago in CONDICION-PAGO-F
          when ER move 1 to w-existe-condicion-pago
                  loop.
       read CONDICION-PAGO-F
       perform CAMBIA-FORMA-PAGO
          when KEND loop.
       move cpa-decto-financiero to w-decto-financiero
       move cpa-dias             to w-dias-vencimiento.

    perform CALCULA-DIA-VENCIMIENTO.
    perform CALCULA-TOTALES-FINALES.
    perform MUESTRA-TOTAL-FINAL
       when KEND loop.
    perform INGRESA-OBSERVACION.
    if w-existe-condicion-pago = 0              ; existe archivo forma de pago
       move cpa-codigo to w-cod-cond-pago.
    if w-cod-cond-pago not = w-cond-pago-efectivo
       perform INGRESA-DOCUMENTOS.
    perform GRABA-TOTALES-FINALES.
    if w-cod-cond-pago = w-cond-pago-efectivo and w-devuelve-credito = 1             ; En un comie
       perform DEVUELVE-CREDITO.
    if w-pvta-autoventa = 0
       perform IMPRIME-FACTURA.
    perform ABRE-REF-PARAMETROS
       when OK seek FIRST in PARAMETROS-F
                  when OK read PARAMETROS-F
;                          add 1 to w-numero-factura
;                          add 1 to par-numero-factura
                          add 1 to par-numero-pedido
                          rewrite REG-PARAMETROS.
            end-when
    perform LEE-PARAMETROS.

    move 0 to w-existe-condicion-pago.               ; por si acaso se inactivo
    perform ABRE-REF-CONDICION-DE-PAGO               ; al comienzo del parrafo
       when ER move 1 to w-existe-condicion-pago.
    seek FIRST in CONDICION-PAGO-F
       when EOF move 1 to w-existe-condicion-pago.
       when OK  read CONDICION-PAGO-F
                move cpa-codigo to w-cond-pago-efectivo.
    exit.

CALCULA-DIA-VENCIMIENTO.
;-----------------------
    message 1 4 'Espere...       '.
    perform LEE-RELOJ.
    move w-fecha to w-fecha-vencimiento.
    move 0       to w-itera-dias.
    if w-dias-vencimiento = 0 exit.
    perform CALCULA-DIAS-0100 UNTIL w-itera-dias >= w-dias-vencimiento.
    exit.

CALCULA-DIAS-0100.
;-----------------
    add 1 to w-itera-dias.
    if w-venc-mes = 2 and w-venc-dia < 28
       add 1 to w-venc-dia
    else
       if w-venc-mes = 2 and w-venc-dia = 28
          perform ANO-BISIESTO
          if w-calculo0 = w-venc-ano
             add 1 to w-venc-dia
          else
             move 1 to w-venc-dia
             add  1 to w-venc-mes
          end-if
       else
          if w-venc-mes = 2 and w-venc-dia = 29
             move 1 to w-venc-dia
             add  1 to w-venc-mes
          else
             perform SIGUE-PREGUNTA-FECHA
          end-if
       end-if
    end-if
    exit.

ANO-BISIESTO.
;-----------
    move w-venc-ano to w-calculo0
    divide 4 into w-calculo0
    multiply w-calculo0 by 4
    exit.

SIGUE-PREGUNTA-FECHA.
;-------------------
    move w-venc-mes to w-indice
    if w-venc-dia < w-arreglo-vencim(w-indice)
       add 1 to w-venc-dia
    else
       if w-venc-mes = 12
          move 1 to w-venc-dia
          move 1 to w-venc-mes
          add  1 to w-venc-ano
       else
          move 1 to w-venc-dia
          add  1 to w-venc-mes
       end-if
    end-if
    exit.

CALCULA-TOTALES-FINALES.
;-----------------------
    move w-total-venta to w-calculo1.
    move w-total-venta to w-calculo2.
    multiply w-calculo1 by w-descuento.
    divide 100 into w-calculo1.

    ; aproxima y saca decimales
    add 50 to w-calculo1.
    divide 100 into w-calculo1.

    subtract w-calculo1 from w-calculo2.
    move w-calculo2 to w-tot-neto.

    move w-tot-neto to w-calculo1.
    move w-tot-neto to w-calculo2.
    multiply w-calculo1 by w-iva.

    ; aproxima y saca decimales
    add 50 to w-calculo1.
    divide 100 into w-calculo1.

    add w-calculo1 to w-calculo2.

    move w-calculo1 to w-tot-iva.
    move w-calculo2 to w-tot-bruto.
    exit.

MUESTRA-TOTAL-FINAL.
;-------------------
    beep.
    move w-descuento to w-mod-descuento.
    show TOTAL-FINAL-SD.
    show DESCUENTO-FINAL-SD.
    fast-action
       when ENTER exit.
       when KEND  exit.
       when CLR   perform PIDE-DESCUENTO-FINAL
                  perform CALCULA-TOTALES-FINALES.
    loop.

PIDE-DESCUENTO-FINAL.
;--------------------
    show TOTAL-FINAL-SD.
    accept DESCUENTO-FINAL-SD
       when OK   if w-descuento > w-decto-financiero
                    beep
                    show DESCUENTO-FINANCIERO-SD
                    wait 15 tenths
                    loop.
                 exit.
       when KEND move w-mod-descuento to w-descuento
                 exit.
    loop.

SEGURO-FIN-VENTA.
;----------------
    beep.
    message 1 1 ' Finaliza Venta '.
    message 1 2 '****************'.
    message 1 3 '    Yes / No    '.
    message 1 4 '                '.
    fast-action
       when ENTER exit.
       when KEND  move ER to last-key exit.
       when NO    move ER to last-key exit.
    loop.

INGRESA-DOCUMENTOS.
;------------------
    move 1           to punt-pagos.
    move w-tot-bruto to w-saldo-pago.
    perform ABRE-UP-PAGOS-VENTA
       when MEM exit.

INGRESA-DOC-01000.
;-----------------
    search FIRST for w-numero-pedido in PAGO-VENTA-F
       when ER next.
    read PAGO-VENTA-F.                        ; Borra todos los anteriores
    delete record from PAGO-VENTA-F.          ; documentos, paar que ingrese
    loop.                                     ; nuevamente

INGRESA-DOC-02000.
;-----------------
    examine punt-pagos
       when 0 move 1 to punt-pagos
              move 0 to w-nro-docum
              move 0 to w-cta-cte
              move 0 to w-monto-pago
              fill w-cod-banco with ' '
              fill w-cod-plaza with ' '
              fill w-nro-serie with ' '
              loop.
       when 1 accept NRO-DOCUM-SD.
       when 2 show NRO-DOCUM-SD
              accept NRO-SERIE-SD.
       when 3 show NRO-DOCUM-SD
              show NRO-SERIE-SD
              accept CTA-CTE-SD.
       when 4 perform ACEPTA-BANCO.
       when 5 perform ACEPTA-PLAZA.
       when 6 clear
              show SALDO-PAGO-SD
              accept MONTO-PAGO-SD.
       when 7 perform CONFIRMA-PAGO.
       when 8 move 1 to punt-pagos loop.
    no-op
       when OK   add 1 to punt-pagos.
       when KEND if punt-pagos = 1
                    perform SEGURO-FIN-PAGOS
                       when ER loop.
                    exit.
                 subtract 1 from punt-pagos.
       when BAR  subtract 1 from punt-pagos.
    loop.

ACEPTA-BANCO.
;------------
    if w-existe-banco = 1                        ; no existe archivo bancos
       move OK to last-key
       exit.
    seek FIRST in BANCO-F.
    read BANCO-F.

ACEPTA-BANCO-0100.
;-----------------
    show BANCO-SD.
    fast-action
       when ENTER move ban-codigo to w-cod-banco
                  move OK to last-key
                  exit.
       when KEND  exit.
       when FAR   seek NEXT in BANCO-F
                     when EOF beep
                              seek LAST in BANCO-F.
                  read BANCO-F.
       when BAR   seek PRIOR in BANCO-F
                     when EOF beep
                              seek FIRST in BANCO-F.
                  read BANCO-F.
    loop.

ACEPTA-PLAZA.
;------------
    if w-existe-banco = 1 or w-existe-plaza = 1 ; no existe arch.banco o  plaza
       move OK to last-key
       exit.
    seek FIRST in PLAZA-F.
    read PLAZA-F.

ACEPTA-PLAZA-0100.
;-----------------
    show BANCO-SD.
    show PLAZA-SD.
    fast-action
       when ENTER move pla-codigo to w-cod-plaza
                  move OK to last-key
                  exit.
       when KEND  exit.
       when FAR   seek NEXT in PLAZA-F
                     when EOF beep
                              seek LAST in PLAZA-F.
                  read PLAZA-F.
       when BAR   seek PRIOR in PLAZA-F
                     when EOF beep
                              seek FIRST in PLAZA-F.
                  read PLAZA-F.
    loop.

CONFIRMA-PAGO.
;-------------
    clear.
    show MONTO-PAGO-SD.
    message 1 3 '** Datos OK ? **'.
    message 1 4 '    Yes / No    '.
    fast-action
       when ENTER next.
       when KEND  exit.
       when NO    move KEND to last-key exit.
    loop.

GRABA-PAGO.
;----------
    beep.
    move w-numero-pedido to pon-numero-pedido.
    move w-nro-docum     to pon-documento.
    move w-nro-serie     to pon-serie.
    move w-cta-cte       to pon-cuenta-cte.
    move w-cod-banco     to pon-banco.
    move w-cod-plaza     to pon-plaza.
    move w-monto-pago    to pon-monto.
    move x'0d'           to pon-cr.
    move x'0a'           to pon-lf.
    write REG-PAGO-VENTA
       when MEM perform MEMORIA-LLENA.
    subtract w-monto-pago from w-saldo-pago.
    exit.

SEGURO-FIN-PAGOS.
;----------------
    beep.
    message 1 1 ' Finaliza Pagos '.
    message 1 2 '****************'.
    message 1 3 '    Yes / No    '.
    message 1 4 '                '.
    fast-action
       when ENTER exit.
       when KEND  move ER to last-key exit.
       when NO    move ER to last-key exit.
    loop.

GRABA-TOTALES-FINALES.
;---------------------
    fill w-compara-obs with ' '.
    move 0 to w-existe-obs.
    perform ABRE-UP-OBSERVACIONES
       when OK if w-observacion not = w-compara-obs
                  search FIRST for w-numero-pedido in OBSERVACIONES-F
                     when OK read OBSERVACIONES-F
                             move 1 to w-existe-obs.
                  move w-numero-pedido to obs-nro-pedido
                  move w-observacion   to obs-observacion
                  move x'0d'           to obs-cr
                  move x'0a'           to obs-lf
                  if w-existe-obs = 0
                     write REG-OBSERVACIONES
                        when MEM perform MEMORIA-LLENA.
                  else
                     rewrite REG-OBSERVACIONES
                  end-if
               end-if
            end-when

    perform ABRE-UP-CABECERA.
    search FIRST for w-numero-pedido in CABECERA-PEDIDO-F
       when ER move 25 to punt-error
               perform MUESTRA-ERROR
               exit.
    read CABECERA-PEDIDO-F.
    move w-total-venta       to cpe-total.
    move w-descuento         to cpe-decto-financiero.
    move w-cod-cond-pago     to cpe-forma-pago.
    move w-fecha-vencimiento to cpe-fecha-vencim
    rewrite REG-CABECERA-PEDIDO.
    exit.

DEVUELVE-CREDITO.
;----------------
    if punt-mp = 3 exit.
    search FIRST for w-cod-cliente in CLIENTES-F
       when ER exit.
    read CLIENTES-F.

    perform ABRE-REF-DETALLE-PEDIDO
       when ER exit.
    search FIRST for w-numero-pedido in DETALLE-PEDIDO-F
       when ER exit.
    read DETALLE-PEDIDO-F.

DEVUELVE-CRED-0100.
;------------------
    if dpe-numero-pedido not = w-numero-pedido
       exit.
    subtract dpe-total-item from cli-deuda-pendiente.

    rewrite REG-CLIENTES.
    search FIRST for w-cod-cliente in CLIENTES-F    ; por si acaso
       when ER exit.                                ; nuevamente lo
    read CLIENTES-F.                                ; posiciono en el cliente

    seek NEXT in DETALLE-PEDIDO-F
       when EOF exit.
    read DETALLE-PEDIDO-F.
    loop.

SCROLL-FAMILIA.
;--------------
    move w-familia to w-miscelaneo02.
    move w-misc22  to w-familia-16.
    show FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-familia to w-miscelaneo03.
    move w-misc32  to w-familia-16.
    show FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-familia to w-miscelaneo04.
    move w-misc42  to w-familia-16.
    show FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-familia to w-miscelaneo05.
    move w-misc52  to w-familia-16.
    show FAMILIA-SD.
    wait 2 tenths.

SCROLL-FAM-0100.
;---------------
    move w-familia to w-miscelaneo06.
    move w-misc62  to w-familia-16.
    show FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-familia to w-miscelaneo07.
    move w-misc72  to w-familia-16.
    show FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-familia to w-miscelaneo08.
    move w-misc82  to w-familia-16.
    show FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-familia to w-miscelaneo09.
    move w-misc92  to w-familia-16.
    show FAMILIA-SD.
    wait 2 tenths.

SCROLL-FAM-0200.
;---------------
    move w-familia  to w-miscelaneo10.
    move w-misc102  to w-familia-16.
    show FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-familia  to w-miscelaneo11.
    move w-misc112  to w-familia-16.
    show FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-familia  to w-miscelaneo12.
    move w-misc122  to w-familia-16.
    show FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-familia  to w-miscelaneo13.
    move w-misc132  to w-familia-16.
    show FAMILIA-SD.
    wait 2 tenths.

SCROLL-FAM-0300.
;---------------
    move w-familia  to w-miscelaneo14.
    move w-misc142  to w-familia-16.
    show FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-familia  to w-miscelaneo15.
    move w-misc152  to w-familia-16.
    show FAMILIA-SD.
    wait 7 tenths.
    exit.

VENTA-02000.
;------------
    perform ABRE-REF-SUB-FAMILIA
       when ER exit.
    seek FIRST in SUB-FAMILIA-F
       when EOF move 5 to punt-error
                perform MUESTRA-ERROR
                exit.
    search FIRST for fam-codigo in SUB-FAMILIA-F OFFSET IS sfa-cod-familia
       when ER move 5 to punt-error
               perform MUESTRA-ERROR
               exit.
    read SUB-FAMILIA-F.

SEL-SUB-FAMILIA.
;---------------
    move sfa-cod-familia to w-busca-familia.
    move sfa-nombre      to w-sub-familia.
    move sfa-nombre      to w-sub-familia-scroll.
    show FAMILIA-SD.
    show SUB-FAMILIA-SD.
    fast-action
       when ENTER perform VENTA-03000.
       when FAR   perform SGTE-SUB-FAMILIA.
       when BAR   perform ANTE-SUB-FAMILIA.
       when UAR   perform SCROLL-SUB-FAMILIA.
       when KEND  exit.
    loop.

SGTE-SUB-FAMILIA.
;----------------
    seek NEXT in SUB-FAMILIA-F
       when EOF beep
                seek LAST in SUB-FAMILIA-F.
    read SUB-FAMILIA-F.
    if sfa-cod-familia not = fam-codigo
       beep
       seek PRIOR in SUB-FAMILIA-F
       read SUB-FAMILIA-F.
    exit.

ANTE-SUB-FAMILIA.
;----------------
    seek PRIOR in SUB-FAMILIA-F
       when EOF beep
                seek FIRST in SUB-FAMILIA-F.
    read SUB-FAMILIA-F.
    if sfa-cod-familia not = fam-codigo
       beep
       seek NEXT in SUB-FAMILIA-F
       read SUB-FAMILIA-F.
    exit.

SCROLL-SUB-FAMILIA.
;------------------
    move w-sub-familia to w-miscelaneo02.
    move w-misc22      to w-sub-familia-16.
    show SUB-FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-sub-familia to w-miscelaneo03.
    move w-misc32      to w-sub-familia-16.
    show SUB-FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-sub-familia to w-miscelaneo04.
    move w-misc42      to w-sub-familia-16.
    show SUB-FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-sub-familia to w-miscelaneo05.
    move w-misc52      to w-sub-familia-16.
    show SUB-FAMILIA-SD.
    wait 2 tenths.

SCROLL-SFA-0100.
;---------------
    move w-sub-familia to w-miscelaneo06.
    move w-misc62      to w-sub-familia-16.
    show SUB-FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-sub-familia to w-miscelaneo07.
    move w-misc72      to w-sub-familia-16.
    show SUB-FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-sub-familia to w-miscelaneo08.
    move w-misc82      to w-sub-familia-16.
    show SUB-FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-sub-familia to w-miscelaneo09.
    move w-misc92      to w-sub-familia-16.
    show SUB-FAMILIA-SD.
    wait 2 tenths.

SCROLL-SFA-0200.
;---------------
    move w-sub-familia  to w-miscelaneo10.
    move w-misc102      to w-sub-familia-16.
    show SUB-FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-sub-familia  to w-miscelaneo11.
    move w-misc112      to w-sub-familia-16.
    show SUB-FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-sub-familia  to w-miscelaneo12.
    move w-misc122      to w-sub-familia-16.
    show SUB-FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-sub-familia  to w-miscelaneo13.
    move w-misc132      to w-sub-familia-16.
    show SUB-FAMILIA-SD.
    wait 2 tenths.

SCROLL-SFA-0300.
;---------------
    move w-sub-familia  to w-miscelaneo14.
    move w-misc142      to w-sub-familia-16.
    show SUB-FAMILIA-SD.
    wait 2 tenths.
    ;
    move w-sub-familia  to w-miscelaneo15.
    move w-misc152      to w-sub-familia-16.
    show SUB-FAMILIA-SD.
    wait 7 tenths.
    exit.

VENTA-03000.
;-----------
    perform ABRE-REF-PRODUCTOS
       when ER exit.
    seek FIRST in PRODUCTOS-F
       when EOF move 7 to punt-error
                perform MUESTRA-ERROR
                exit.
    move fam-codigo     to w-busca-cod-familia.
    move sfa-cod-subfam to w-busca-cod-sub-fam.
    search FIRST for w-busca-familias in PRODUCTOS-F OFFSET IS pro-busca-familias
       when ER move 12 to punt-error
               perform MUESTRA-ERROR
               exit.
    read PRODUCTOS-F.
    move 0 to w-largo-nombre.

SEL-PRODUCTO.
;------------
    message 1 4 'Espere ...      '.
    move pro-codigo      to w-busca-producto.
    move pro-descripcion to w-nombre-producto.
    move 0               to w-precio.
    move 0               to w-existe-precio.
    move 0               to w-es-modificacion.
    search FIRST for pro-codigo in LISTA-PRECIO-F
       when OK read LISTA-PRECIO-F
               move lpr-precio to w-precio
               move 1          to w-existe-precio.
    show SEL-PRODUCTO-SD.
    fast-action
       when ENTER move 0 to w-largo-nombre
                  if w-existe-precio = 0
                     move 17 to punt-error
                     perform MUESTRA-ERROR
                     loop.
                  perform VENTA-04000.
       when FAR   perform SGTE-PRODUCTO
                  move 0 to w-largo-nombre.
       when BAR   perform ANTE-PRODUCTO
                  move 0 to w-largo-nombre.
       when CLR   perform PIDE-CODIGO-PRODUCTO
                  move 0 to w-largo-nombre.
       when KNEXT perform UNIDAD-MEDIDA
                  move 0 to w-largo-nombre.
       when KNOMB move 0 to w-largo-nombre
                  perform PIDE-NOMBRE-PRODUCTO.
       when KSGTE perform PIDE-SGTE-PRODUCTO.
       when UAR   show SCROLL-PRODUCTO-SD
                  wait 7 tenths.
       when KEND  exit.
    loop.

SGTE-PRODUCTO.
;-------------
    seek NEXT in PRODUCTOS-F
       when EOF beep
                seek LAST in PRODUCTOS-F.
    read PRODUCTOS-F.
    if pro-familia not = fam-codigo or pro-sub-familia not = sfa-cod-subfam
       beep
       seek PRIOR in PRODUCTOS-F
       read PRODUCTOS-F.
    exit.

ANTE-PRODUCTO.
;-------------
    seek PRIOR in PRODUCTOS-F
       when EOF beep
                seek FIRST in PRODUCTOS-F.
    read PRODUCTOS-F.
    if pro-familia not = fam-codigo or pro-sub-familia not = sfa-cod-subfam
       beep
       seek NEXT in PRODUCTOS-F
       read PRODUCTOS-F.
    exit.

PIDE-CODIGO-PRODUCTO.
;--------------------
    fill w-codigo-producto with ' '.
    move 0 to w-largo-cod-prod.
    accept CODIGO-PRODUCTO-SD
       when KEND perform RECUPERA-FAMILIAS exit.
       when OK   move entry-size to w-largo-cod-prod
                 if w-largo-cod-prod = 0 loop.
                 if w-largo-cod-prod not = 6
                    perform UBICA-FAMILIA-Y-SUB-FAMILIA
                       when ER loop.
                 else
                    move w-codigo-producto to w-cod-prod-tmp
                    fill w-codigo-producto with ' '
                    move w-cod-prod-tmp    to w-cod-prod-resto
                 end-if
                 ; busca el codigo del producto
                 if w-largo-cod-prod = 4
                    search FIRST for w-cod-familias in PRODUCTOS-F
                 else
                    if w-largo-cod-prod = 6
                       search FIRST for w-cod-prod-resto in PRODUCTOS-F OFFSET IS pro-producto
                    else
                       search FIRST for w-codigo-producto in PRODUCTOS-F.
                 end-if
                 no-op
                    when OK read PRODUCTOS-F
                            if w-largo-cod-prod = 6
                               perform UBICA-FAMILIA-Y-SUB-FAMILIA
                                  when ER loop.
                            end-if
                            move fam-codigo     to w-busca-cod-familia
                            move sfa-cod-subfam to w-busca-cod-sub-fam
                            exit.
                 search FIRST for w-busca-producto in PRODUCTOS-F
                 read PRODUCTOS-F
                 move 9 to punt-error
                 perform MUESTRA-ERROR.
    loop.

UBICA-FAMILIA-Y-SUB-FAMILIA.
;---------------------------
    if w-largo-cod-prod = 6
       move pro-familia     to w-cod-prod-familia
       move pro-sub-familia to w-cod-prod-sub-fam.
    ; busca la familia del producto
    search FIRST for w-cod-prod-familia in FAMILIA-F
       when ER move 13 to punt-error
               perform MUESTRA-ERROR
               exit.
    read FAMILIA-F.
    move fam-codigo to w-cod-prod-familia.
    move fam-nombre to w-familia.
    move fam-nombre to w-familia-scroll.

    ; busca la sub-familia del producto
    search FIRST for w-cod-familias in SUB-FAMILIA-F
       when ER move 14 to punt-error
               perform MUESTRA-ERROR
               exit.
    read SUB-FAMILIA-F.
    move sfa-cod-subfam to w-cod-prod-sub-fam.
    exit.

RECUPERA-FAMILIAS.
;-----------------
    ; busca la familia original del producto
    search FIRST for w-busca-cod-familia in FAMILIA-F
       when ER move 22 to punt-error
               perform MUESTRA-ERROR
               exit.
    read FAMILIA-F
    move fam-codigo to w-cod-prod-familia.
    move fam-nombre to w-familia.
    move fam-nombre to w-familia-scroll.

    ; busca la sub-familia original del producto
    search FIRST for w-busca-familias in SUB-FAMILIA-F
       when ER move 23 to punt-error
               perform MUESTRA-ERROR
               exit.
    read SUB-FAMILIA-F.
    move sfa-cod-subfam to w-cod-prod-sub-fam.
    exit.

UNIDAD-MEDIDA.
;-------------
    show SEL-PRODUCTO-SD.
    show UNIDAD-MEDIDA-SD.
    fast-action
       when ENTER exit.
       when KNEXT exit.
       when KEND  exit.
       when UAR   show SCROLL-PRODUCTO-SD
                  wait 7 tenths.
       when FAR   perform SGTE-PRODUCTO.
       when BAR   perform ANTE-PRODUCTO.
    loop.

PIDE-NOMBRE-PRODUCTO.
;--------------------
    move 0 to w-largo-nombre.
    accept SCROLL-PRODUCTO-SD
       when OK   move entry-size to w-largo-nombre
                 if w-largo-nombre = 0 beep, loop.
                 perform PIDE-PRODUC-0100
                 perform PIDE-PRODUC-0500
                 exit.
       when KEND exit.
    loop.

PIDE-PRODUC-0100.
;----------------
    message 1 4 'Espere ...      '.
    move w-nombre-producto to w-producto-01.
    move w-nombre-producto to w-producto-02.
    move w-nombre-producto to w-producto-03.
    move w-nombre-producto to w-producto-04.
    move w-nombre-producto to w-producto-05.
    move w-nombre-producto to w-producto-06.
    move w-nombre-producto to w-producto-07.
    move w-nombre-producto to w-producto-08.
    move w-nombre-producto to w-producto-09.
    move w-nombre-producto to w-producto-10.
    move w-nombre-producto to w-producto-11.
    move w-nombre-producto to w-producto-12.
    move w-nombre-producto to w-producto-13.
    move w-nombre-producto to w-producto-14.
    move w-nombre-producto to w-producto-15.
    move w-nombre-producto to w-producto-16.
    move w-nombre-producto to w-producto-17.
    move w-nombre-producto to w-producto-18.
    move w-nombre-producto to w-producto-19.
    move w-nombre-producto to w-producto-20.
    move w-nombre-producto to w-producto-21.
    move w-nombre-producto to w-producto-22.
    move w-nombre-producto to w-producto-23.
    move w-nombre-producto to w-producto-24.
    move w-nombre-producto to w-producto-25.
    move w-nombre-producto to w-producto-26.
    move w-nombre-producto to w-producto-27.
    move w-nombre-producto to w-producto-28.
    move w-nombre-producto to w-producto-29.

PIDE-PRODUC-0200.
;----------------
    if w-largo-nombre > 9 next.
    examine w-largo-nombre
       when 1 search FIRST for w-producto-01 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 2 search FIRST for w-producto-02 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 3 search FIRST for w-producto-03 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 4 search FIRST for w-producto-04 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 5 search FIRST for w-producto-05 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 6 search FIRST for w-producto-06 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 7 search FIRST for w-producto-07 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 8 search FIRST for w-producto-08 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 9 search FIRST for w-producto-09 in PRODUCTOS-F OFFSET IS pro-descripcion.
    no-op
       when OK read PRODUCTOS-F.
    exit.

PIDE-PRODUC-0300.
;----------------
    if w-largo-nombre > 19 next.
    examine w-largo-nombre
       when 10 search FIRST for w-producto-10 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 11 search FIRST for w-producto-11 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 12 search FIRST for w-producto-12 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 13 search FIRST for w-producto-13 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 14 search FIRST for w-producto-14 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 15 search FIRST for w-producto-15 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 16 search FIRST for w-producto-16 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 17 search FIRST for w-producto-17 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 18 search FIRST for w-producto-18 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 19 search FIRST for w-producto-19 in PRODUCTOS-F OFFSET IS pro-descripcion.
    no-op
       when OK read PRODUCTOS-F.
    exit.

PIDE-PRODUC-0400.
;----------------
    examine w-largo-nombre
       when 20 search FIRST for w-producto-20     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 21 search FIRST for w-producto-21     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 22 search FIRST for w-producto-22     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 23 search FIRST for w-producto-23     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 24 search FIRST for w-producto-24     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 25 search FIRST for w-producto-25     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 26 search FIRST for w-producto-26     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 27 search FIRST for w-producto-27     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 28 search FIRST for w-producto-28     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 29 search FIRST for w-producto-29     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 30 search FIRST for w-nombre-producto in PRODUCTOS-F OFFSET IS pro-descripcion.
    no-op
       when OK read PRODUCTOS-F.
    exit.

PIDE-PRODUC-0500.
;----------------
    no-op                      ; No Encontro producto ingresado
       when ER search FIRST for w-busca-producto in PRODUCTOS-F
               read PRODUCTOS-F
               move 9 to punt-error
               perform MUESTRA-ERROR
               exit.

    move pro-codigo to w-codigo-producto.
    search FIRST for w-cod-prod-familia in FAMILIA-F
       when ER move 13 to punt-error
               perform MUESTRA-ERROR
               loop.
    read FAMILIA-F.
    move fam-codigo to w-cod-prod-familia.
    move fam-nombre to w-familia.
    move fam-nombre to w-familia-scroll.

     ; busca la sub-familia del producto
    search FIRST for w-cod-familias in SUB-FAMILIA-F
       when ER move 14 to punt-error
               perform MUESTRA-ERROR
               loop.
    read SUB-FAMILIA-F.
    move sfa-cod-subfam to w-cod-prod-sub-fam.

    exit.

PIDE-SGTE-PRODUCTO.
;------------------
    if w-largo-nombre = 0
       beep
       exit.
    perform PIDE-SGTE-PRO-0100.
    perform PIDE-PRODUC-0500.
    exit.

PIDE-SGTE-PRO-0100.
;------------------
    message 1 4 'Espere ...      '.
    move w-nombre-producto to w-producto-01.
    move w-nombre-producto to w-producto-02.
    move w-nombre-producto to w-producto-03.
    move w-nombre-producto to w-producto-04.
    move w-nombre-producto to w-producto-05.
    move w-nombre-producto to w-producto-06.
    move w-nombre-producto to w-producto-07.
    move w-nombre-producto to w-producto-08.
    move w-nombre-producto to w-producto-09.
    move w-nombre-producto to w-producto-10.
    move w-nombre-producto to w-producto-11.
    move w-nombre-producto to w-producto-12.
    move w-nombre-producto to w-producto-13.
    move w-nombre-producto to w-producto-14.
    move w-nombre-producto to w-producto-15.
    move w-nombre-producto to w-producto-16.
    move w-nombre-producto to w-producto-17.
    move w-nombre-producto to w-producto-18.
    move w-nombre-producto to w-producto-19.
    move w-nombre-producto to w-producto-20.
    move w-nombre-producto to w-producto-21.
    move w-nombre-producto to w-producto-22.
    move w-nombre-producto to w-producto-23.
    move w-nombre-producto to w-producto-24.
    move w-nombre-producto to w-producto-25.
    move w-nombre-producto to w-producto-26.
    move w-nombre-producto to w-producto-27.
    move w-nombre-producto to w-producto-28.
    move w-nombre-producto to w-producto-29.

PIDE-SGTE-DIR-0200.
;------------------
    if w-largo-nombre > 9 next.
    examine w-largo-nombre
       when 1 search NEXT for w-producto-01 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 2 search NEXT for w-producto-02 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 3 search NEXT for w-producto-03 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 4 search NEXT for w-producto-04 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 5 search NEXT for w-producto-05 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 6 search NEXT for w-producto-06 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 7 search NEXT for w-producto-07 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 8 search NEXT for w-producto-08 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 9 search NEXT for w-producto-09 in PRODUCTOS-F OFFSET IS pro-descripcion.
    no-op
       when OK read PRODUCTOS-F.
    exit.

PIDE-SGTE-PRO-0300.
;------------------
    if w-largo-nombre > 19 next.
    examine w-largo-nombre
       when 10 search NEXT for w-producto-10 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 11 search NEXT for w-producto-11 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 12 search NEXT for w-producto-12 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 13 search NEXT for w-producto-13 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 14 search NEXT for w-producto-14 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 15 search NEXT for w-producto-15 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 16 search NEXT for w-producto-16 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 17 search NEXT for w-producto-17 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 18 search NEXT for w-producto-18 in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 19 search NEXT for w-producto-19 in PRODUCTOS-F OFFSET IS pro-descripcion.
    no-op
       when OK read PRODUCTOS-F.
    exit.

PIDE-SGTE-PRO-0400.
;------------------
    examine w-largo-nombre
       when 20 search NEXT for w-producto-20     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 21 search NEXT for w-producto-21     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 22 search NEXT for w-producto-22     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 23 search NEXT for w-producto-23     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 24 search NEXT for w-producto-24     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 25 search NEXT for w-producto-25     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 26 search NEXT for w-producto-26     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 27 search NEXT for w-producto-27     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 28 search NEXT for w-producto-28     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 29 search NEXT for w-producto-29     in PRODUCTOS-F OFFSET IS pro-descripcion.
       when 30 search NEXT for w-nombre-producto in PRODUCTOS-F OFFSET IS pro-descripcion.
    no-op
       when OK read PRODUCTOS-F.
    exit.

VENTA-04000.
;-----------
    perform ABRE-UP-DETALLE-PEDIDO
       when MEM exit.
    move w-numero-pedido to w-llave1-detalle.
    move pro-codigo      to w-llave2-detalle.
    move 0               to w-item-antiguo.
    search FIRST for w-llave-detalle in DETALLE-PEDIDO-F
       when OK if punt-venta = 2                  ; Correccion del pedido
                  move 33 to punt-error
                  perform MUESTRA-ERROR
                  exit.
               read DETALLE-PEDIDO-F
               move dpe-cantidad   to w-cantidad
               move dpe-descuento  to w-descuento
               move dpe-total-item to w-item-antiguo
               move 0              to w-bonificacion
               move 1              to w-es-modificacion
               perform ABRE-REF-BONIFICACION
                  when OK search FIRST for w-llave-detalle in BONIFICACION-F
                             when OK read BONIFICACION-F
                                     move bon-cantidad to w-bonificacion.
                       end-when
               perform VENTA-05000
               exit.
    if w-cuenta-itemes >= w-maximo-itemes
       move 16 to punt-error
       perform MUESTRA-ERROR
       exit.

VENTA-04100.
;-----------
    move 0 to w-cantidad.
    move 0 to w-bonificacion.
    accept CANTIDAD-SD
       when OK   if w-cantidad = 0 loop.
                 move pro-stock to w-stock
                 subtract w-cantidad from w-stock
                 if w-stock < 0
                    beep
                    add w-cantidad to w-stock
                    show FALTA-STOCK-SD
                    wait 1 second
                    if w-pvta-autoventa = 0    ; es autoventa
                       loop.
                    subtract w-cantidad from w-stock.
                 perform VENTA-04200
                    when KEND loop.
                 exit.
       when KEND exit.
    loop.

VENTA-04200.
;-----------
    if w-cantidad >= lpr-cant-bonifica              ; hay bonificacion
       if w-pvta-autoventa = 0           ; autoventa
          add 1 to w-cuenta-itemes
          if w-cuenta-itemes >= w-maximo-itemes
             subtract 1 from w-cuenta-itemes
             move 24 to punt-error
             perform MUESTRA-ERROR
             next
          end-if
          subtract 1 from w-cuenta-itemes
       end-if
       perform INGRESA-BONIFICACION
          when KEND exit.
    end-if

VENTA-04300.
;-----------
    move 0 to w-descuento.
    if lpr-descuento <= 0                           ; no hay descuento
       perform CALC-TOT-ITEM
       exit.
    show CANTIDAD-SD.
    show ING-BONIFICACION-SD.
    show TOPE-DESCUENTO-SD.
    accept DESCUENTO-SD
       when OK   if w-descuento > lpr-descuento
                    beep
                    loop.
                 perform CALC-TOT-ITEM
                 exit.
       when KEND exit.
    loop.

INGRESA-BONIFICACION.
;--------------------
    move w-cantidad to w-tope-bonificacion.
    divide lpr-cant-bonifica into w-tope-bonificacion.
    multiply w-tope-bonificacion by lpr-bonificacion.

ING-BONIF-01000.
;---------------
    move 0 to w-bonificacion.
    show CANTIDAD-SD.
    show MUE-BONIFICACION-SD.
    accept ING-BONIFICACION-SD
       when OK   if w-bonificacion > w-tope-bonificacion
                    beep
                    loop.
                 subtract w-bonificacion from w-stock
                 if w-stock < 0
                    beep
                    add w-bonificacion to w-stock
                    show FALTA-STOCK-SD
                    wait 1 second
                    if w-pvta-autoventa = 0    ; es autoventa
                       loop.
                    subtract w-bonificacion from w-stock.
                 exit.
       when KEND exit.
    loop.

CALC-TOT-ITEM.
;-------------
    perform CALCULA-SUB-TOTAL.

CALC-TOT-ITEM-0100.
;------------------
    show CANTIDAD-SD.
    show ING-BONIFICACION-SD.
    show DESCUENTO-SD.
    show SUB-TOTAL-SD.
    fast-action
       when ENTER perform VENTA-06000 exit.
       when KNEXT if punt-venta = 2 loop.
                  perform MUESTRA-TOT-VENTA.
       when KEND  subtract w-total-item from w-total-venta
                  if w-es-modificacion = 1              ; es modificacion
                     add w-item-antiguo to w-total-venta.
                  exit.
    loop.

MUESTRA-TOT-VENTA.
;-----------------
    show CANTIDAD-SD.
    show ING-BONIFICACION-SD.
    show TOTAL-VENTA-SD.
    fast-action
       when ENTER exit.
       when KNEXT exit.
       when KEND  exit.
    loop.

CALCULA-SUB-TOTAL.
;-----------------
    move w-precio    to w-calculo1.
    move w-cantidad  to w-calculo2.
    move w-descuento to w-calculo3.

    ; w-calculo1 = precio * cantidad
    multiply w-calculo1 by   w-calculo2.
    divide   100        into w-calculo1.

    ; w-calculo2 = w-calculo1 * descuento
    ;              ($ * cant)
    multiply w-calculo3 by   w-calculo1.
    divide   10000      into w-calculo3.
    subtract w-calculo3 from w-calculo1.

    ; aproximo el total del item y le saco los decimales
    add 50 to w-calculo1.
    divide 100 into w-calculo1.

    move w-calculo1 to w-total-item.

    if w-es-modificacion = 1                           ; es modificacion
       subtract w-item-antiguo from w-total-venta.

    add w-total-item to w-total-venta.
    exit.

VENTA-05000.                          ; revisa el item si es que ya existia.
;-----------
    show CANTIDAD-SD.                 ; revisa para modificar cantidad
    show ING-BONIFICACION-SD.
    show DESCUENTO-SD.
    message 9 1 ':'.
    SET-CURSOR to BLOCK with BLINK.
    fast-action
       when ENTER perform VENTA-05200 exit.
       when DIG   perform VENTA-05100
                     when KEND exit.
                     when ER   loop.
                  perform VENTA-05200
                  exit.
       when KDEL  SET-CURSOR to NONE
                  perform BORRA-ITEM
                     when KEND loop.
                  exit.
       when KEND  SET-CURSOR to NONE, exit.
    loop.

VENTA-05100.
;-----------
    move 0 to w-mod-cantidad.
    SET-CURSOR to NONE.
    clear.
    show ING-BONIFICACION-SD.
    show DESCUENTO-SD.
    accept CANTIDAD2-SD
       when OK   if w-mod-cantidad = 0 loop.
                 move     pro-stock      to   w-stock
                 add      w-cantidad     to   w-stock
                 subtract w-mod-cantidad from w-stock
                 if w-stock < 0
                    beep
                    move pro-stock to w-stock
                    show FALTA-STOCK-SD
                    wait 2 seconds
                    if w-pvta-autoventa = 0    ; es autoventa
                       move ER to last-key
                       exit.
                    add      w-cantidad     to   w-stock
                    subtract w-mod-cantidad from w-stock.
                 move w-mod-cantidad to w-cantidad
                 exit.
       when KEND exit.
    loop.

VENTA-05200.
;-----------
    if w-cantidad < lpr-cant-bonifica
       move 0 to w-bonificacion
       perform VENTA-05400
       exit.
    show CANTIDAD-SD.                 ; revisa para modificar bonificacion
    show ING-BONIFICACION-SD.
    show DESCUENTO-SD.
    message 9 2 ':'.
    SET-CURSOR to BLOCK with BLINK.
    fast-action
       when ENTER perform VENTA-05400 exit.
       when DIG   perform VENTA-05300
                     when KEND exit.
                     when ER   loop.
                  perform VENTA-05400
                  exit.
       when KDEL  SET-CURSOR to NONE
                  perform BORRA-ITEM
                     when KEND loop.
                  exit.
       when KEND  SET-CURSOR to NONE, exit.
    loop.

VENTA-05300.
;-----------
    move w-cantidad to w-tope-bonificacion.
    divide lpr-cant-bonifica into w-tope-bonificacion.
    multiply w-tope-bonificacion by lpr-bonificacion.

VENTA-05301.
;-----------
    move 0 to w-mod-bonificacion.
    SET-CURSOR to NONE.
    show CANTIDAD-SD.
    show DESCUENTO-SD.
    accept ING-BONIF2-SD
       when OK  if w-mod-bonificacion > w-tope-bonificacion
                   beep
                   show MUE-BONIFICACION-SD
                   wait 1 second
                   move ER to last-key
                   exit.
                 move     pro-stock          to   w-stock
                 add      w-bonificacion     to   w-stock
                 subtract w-mod-bonificacion from w-stock
                 if w-stock < 0
                    beep
                    move pro-stock to w-stock
                    show FALTA-STOCK-SD
                    wait 1 second
                    if w-pvta-autoventa = 0    ; es autoventa
                       move ER to last-key
                       exit.
                    add      w-bonificacion     to   w-stock
                    subtract w-mod-bonificacion from w-stock.
                 move w-mod-bonificacion to w-bonificacion
                 exit.
       when KEND exit.
    loop.

VENTA-05400.
;-----------
    if lpr-descuento <= 0                           ; no hay descuento
       SET-CURSOR to NONE
       move 0 to w-descuento
       perform CALC-TOT-ITEM
       exit.

VENTA-05450.
;-----------
    show CANTIDAD-SD.                 ; revisa para modificar descuento
    show ING-BONIFICACION-SD.
    show DESCUENTO-SD.
    message 10 3 ':'.
    SET-CURSOR to BLOCK with BLINK.
    fast-action
       when ENTER SET-CURSOR to NONE
                  perform CALC-TOT-ITEM exit.
       when DIG   perform VENTA-05500
                     when KEND exit.
                     when ER   loop.
                  perform CALC-TOT-ITEM
                  exit.
       when KDEL  SET-CURSOR to NONE
                  perform BORRA-ITEM
                     when KEND loop.
                  exit.
       when KEND  SET-CURSOR to NONE, exit.
    loop.

VENTA-05500.
;-----------
    move 0 to w-mod-descuento.
    SET-CURSOR to NONE.
    show CANTIDAD-SD.
    show ING-BONIFICACION-SD.
    show TOPE-DESCUENTO-SD.
    accept DESCUENTO2-SD
       when OK   if w-mod-descuento > lpr-descuento
                    beep
                    exit.
                 move w-mod-descuento to w-descuento
                 exit.
       when KEND exit.
    loop.

BORRA-ITEM.
;----------
    beep.
    message 1 1 '****************'.
    message 1 2 ' Seguro Elimina '.
    message 1 3 '    Yes / No    '.
    message 1 4 '****************'.
    fast-action
       when ENTER next.
       when NO    move KEND to last-key exit.
       when KEND  exit.
    loop.

BORRA-ITEM-010.
;--------------
    message 1 1 '****************'.
    message 1 2 ' Presione ERASE '.
    message 1 3 '   Por 3 Beep   '.
    message 1 4 '****************'.
    fast-action
       when KDEL 3-beep
                    when OK next.
            end-when
    move KEND to last-key.
    exit.

BORRA-ITEM-020.
;--------------
    add dpe-cantidad to pro-stock.
    rewrite REG-PRODUCTOS.

    if w-cod-cond-pago not = w-cond-pago-efectivo
       subtract dpe-total-item from cli-deuda-pendiente
       rewrite REG-CLIENTES.

    subtract 1              from w-cuenta-itemes.
    subtract dpe-total-item from w-total-venta.
    add      dpe-total-item to   w-credito-venta.
    delete RECORD from DETALLE-PEDIDO-F.
    search FIRST for w-numero-pedido in DETALLE-PEDIDO-F
       when ER perform ABRE-UP-CABECERA-PEDIDO
                  when MEM exit.
               search FIRST for w-numero-pedido in CABECERA-PEDIDO-F
                  when OK read CABECERA-PEDIDO-F
                          delete RECORD from CABECERA-PEDIDO-F
                          move 0 to w-total-venta
                          move 0 to w-fin-venta.
            end-when

    perform ABRE-UP-BONIFICACION
       when MEM exit.
    search FIRST for w-llave-detalle in BONIFICACION-F
       when OK read BONIFICACION-F
               add bon-cantidad to pro-stock
               subtract 1 from w-cuenta-itemes
               rewrite REG-PRODUCTOS
               delete RECORD from BONIFICACION-F.
    exit.

VENTA-06000.
;-----------
    if w-cod-cond-pago not = w-cond-pago-efectivo
       move 1 to w-devuelve-credito
       if w-es-modificacion = 1                       ; es modificacion de item
          add w-item-antiguo to w-credito-venta.
       subtract w-total-item from w-credito-venta
       if w-credito-venta < 0 and w-pvta-autoventa not = 0   ; no es autoventa
          add w-total-item to w-credito-venta
          if w-es-modificacion = 1                    ; es modificacion de item
             subtract w-item-antiguo from w-credito-venta.
          perform SIN-CREDITO
          subtract w-total-item from w-total-venta
          exit.
    end-if
    perform VENTA-06050.
    exit.

SIN-CREDITO.
;-----------
    alarm.
    show NO-HAY-MAS-CREDITO.
    fast-action
       when ENTER exit.
       when KEND  exit.
    loop.

VENTA-06050.
;-----------
    beep
    if w-es-modificacion = 1         ; es modificacion de item
       show RECONFIRMA-ITEM-SD
    else
       show CONFIRMA-ITEM-SD.
    message 5 4 'Yes / No'.
    fast-action
       when ENTER perform VENTA-06100
                  perform SGTE-PRODUCTO
                  exit.
       when YES   perform VENTA-06100
                  perform SGTE-PRODUCTO
                  exit.
       when NO    subtract w-total-item from w-total-venta
                  add      w-total-item to   w-credito-venta
                  if w-es-modificacion = 1               ; es modificacion
                     add w-item-antiguo to w-total-venta.
                  exit.
       when KEND  subtract w-total-item from w-total-venta
                  add      w-total-item to   w-credito-venta
                  if w-es-modificacion = 1               ; es modificacion
                     add w-item-antiguo to w-total-venta.
                  exit.
    loop.

VENTA-06100.
;-----------
    if w-es-modificacion = 0                   ; no es modificacion,es ingreso
       subtract w-bonificacion from pro-stock
       subtract w-cantidad     from pro-stock
       rewrite REG-PRODUCTOS
       if w-fin-venta = 0 and punt-venta not = 2 ; graba cabec.y no es correccion
          perform VENTA-06200.
    else
       move w-stock to pro-stock
       rewrite REG-PRODUCTOS
    end-if

    perform ABRE-UP-DETALLE-PEDIDO             ; graba detalle
       when MEM exit.
    move 0 to w-existe-detalle.
    move 0 to w-item-antiguo.
    if punt-venta = 2                   ; agrega item en correccion
       perform VENTA-07000
       exit.
    search FIRST for w-llave-detalle in DETALLE-PEDIDO-F
       when OK read DETALLE-PEDIDO-F
               move dpe-total-item to w-item-antiguo
               move 1              to w-existe-detalle.
       when ER fill REG-DETALLE-PEDIDO with ' '.

    move w-numero-pedido to dpe-numero-pedido.
    move pro-codigo      to dpe-producto.
    move w-cantidad      to dpe-cantidad.
    move w-total-item    to dpe-total-item.
    move w-descuento     to dpe-descuento.
    move x'0d'           to dpe-cr.
    move x'0a'           to dpe-lf.
    if w-existe-detalle = 1
       rewrite REG-DETALLE-PEDIDO
    else
       write REG-DETALLE-PEDIDO
          when MEM perform MEMORIA-LLENA.
       if w-pvta-autoventa = 0                 ; es autoventa
          add 1 to w-cuenta-itemes.
    end-if
    perform REBAJA-CREDITO-CLIENTE.

    if w-bonificacion = 0
       if w-es-modificacion = 1                   ; es modificacion de item
          perform ABRE-UP-BONIFICACION            ; y antes habia bonificacion,
             when MEM exit.                       ; entonces ahora se borra.
          search FIRST for w-llave-detalle in BONIFICACION-F
             when OK read BONIFICACION-F
                     delete record from BONIFICACION-F
                     subtract 1 from w-cuenta-itemes.
       end-if
       exit.

    perform ABRE-UP-BONIFICACION               ; graba bonificacion
       when MEM exit.
    move 0 to w-existe-detalle.
    search FIRST for w-llave-detalle in BONIFICACION-F
       when OK read BONIFICACION-F
               move 1 to w-existe-detalle.
       when ER if w-es-modificacion = 1           ; es modificacion de item
                  if w-cuenta-itemes >= w-maximo-itemes
                     move 24 to punt-error
                     perform MUESTRA-ERROR
                     exit
                  end-if
               end-if
               fill REG-BONIFICACION with ' '.

    move w-numero-pedido to bon-numero-pedido.
    move pro-codigo      to bon-producto.
    move w-bonificacion  to bon-cantidad.
    move x'0d'           to bon-cr.
    move x'0a'           to bon-lf.
    if w-existe-detalle = 1
       rewrite REG-BONIFICACION
    else
       write REG-BONIFICACION
          when MEM perform MEMORIA-LLENA.
       if w-pvta-autoventa = 0                 ; es autoventa
          add 1 to w-cuenta-itemes.
    end-if
    exit.

VENTA-06200.
;-----------
    perform ABRE-UP-CABECERA-PEDIDO
       when MEM exit.
    perform LEE-RELOJ.
    fill REG-CABECERA-PEDIDO with ' '.
    move 1 to w-fin-venta.
    move w-numero-pedido  to cpe-numero-pedido.
    if w-pvta-autoventa = 0
       move w-numero-factura to cpe-numero-factura
    else
       move 0                to cpe-numero-factura
    end-if
    move w-cod-cliente    to cpe-cliente.
    move w-lista-precio   to cpe-lista-precio.
    move w-fecha          to cpe-fecha.
    move w-hora           to cpe-hora.
    move w-bodega         to cpe-bodega.
    move x'0d'            to cpe-cr.
    move x'0a'            to cpe-lf.
    write REG-CABECERA-PEDIDO
       when MEM perform MEMORIA-LLENA.
    exit.

VENTA-07000.
;-----------
    move 0 to w-existe-detalle.
    search FIRST for w-numero-pedido in DETALLE-PEDIDO-F
       when OK read DETALLE-PEDIDO-F
               move 1 to w-existe-detalle.
       when ER fill REG-DETALLE-PEDIDO with ' '.

    move w-numero-pedido to dpe-numero-pedido.
    move pro-codigo      to dpe-producto.
    move w-cantidad      to dpe-cantidad.
    move w-total-item    to dpe-total-item.
    move w-descuento     to dpe-descuento.
    move x'0d'           to dpe-cr.
    move x'0a'           to dpe-lf.
    if w-existe-detalle = 1
       insert REG-DETALLE-PEDIDO
    else
       write REG-DETALLE-PEDIDO
          when MEM perform MEMORIA-LLENA.
    end-if
    if w-pvta-autoventa = 0                 ; es autoventa
       add 1 to w-cuenta-itemes.
    perform REBAJA-CREDITO-CLIENTE.

    if w-bonificacion = 0
       if w-es-modificacion = 1                   ; es modificacion de item
          perform ABRE-UP-BONIFICACION            ; y antes habia bonificacion,
             when MEM exit.                       ; entonces ahora se borra.
          search FIRST for w-llave-detalle in BONIFICACION-F
             when OK read BONIFICACION-F
                     delete record from BONIFICACION-F
                     subtract 1 from w-cuenta-itemes.
       end-if
       exit.

    perform ABRE-UP-BONIFICACION               ; graba bonificacion
       when MEM exit.
    move 0 to w-existe-detalle.
    search FIRST for w-numero-pedido in BONIFICACION-F
       when OK read BONIFICACION-F
               move 1 to w-existe-detalle.
       when ER fill REG-BONIFICACION with ' '.

    move w-numero-pedido to bon-numero-pedido.
    move pro-codigo      to bon-producto.
    move w-bonificacion  to bon-cantidad.
    move x'0d'           to bon-cr.
    move x'0a'           to bon-lf.
    if w-existe-detalle = 1
       insert REG-BONIFICACION
    else
       write REG-BONIFICACION
          when MEM perform MEMORIA-LLENA.
    end-if
    if w-pvta-autoventa = 0                 ; es autoventa
       add 1 to w-cuenta-itemes.
    exit.

REBAJA-CREDITO-CLIENTE.
;----------------------
    if w-cod-cond-pago = w-cond-pago-efectivo exit.       ; cliente nuevo
    if w-existe-detalle = 1
       subtract w-item-antiguo from cli-deuda-pendiente.
    add w-total-item to cli-deuda-pendiente.
    rewrite REG-CLIENTES.
    exit.

;ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
;ณ                          IMPRESION DE LA FACTURA                           ณ
;ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
IMPRIME-FACTURA.
;---------------
    message 1 1 'Imprime Factura?'.
    message 1 2 '****************'.
    message 1 3 '                '.
    message 1 4 '    Yes / No    '.
    fast-action
       when ENTER next.
       when KEND  exit.
       when NO    exit.
    loop.

IMPRIME-01000.
;-------------
    move w-numero-factura to w-nro-factura2.
    show NRO-FACTURA-SD
    fast-action
       when ENTER perform IMPRIME-02000
                     when KEND loop.
                  perform IMPRESION-OK
                     when ER if punt-venta = 1    ; no es modificacion
                                add 1 to w-numero-factura
                                move w-numero-factura to par-numero-factura
                                rewrite REG-PARAMETROS.
                             loop.
                  if punt-venta = 1       ; no es modificacion
                     add 1 to w-numero-factura
                     move w-numero-factura to par-numero-factura
                     rewrite REG-PARAMETROS.
                  exit.
       when KEND  exit.
;      when CLR   perform CAMBIA-NRO-FACTURA.
    loop.

;CAMBIA-NRO-FACTURA.
;;------------------
;    accept NRO-FACTURA-SD
;       when KEND move w-nro-factura2 to w-numero-factura
;                 exit.
;       when OK   if entry-size = 0
;                    beep
;                    loop.
;                 move w-numero-factura to par-numero-factura
;                 rewrite REG-PARAMETROS
;                 exit.
;    loop.

IMPRESION-OK.
;------------
    message 1 1 ' Impresion OK ? '.
    message 1 2 '****************'.
    message 1 3 '                '.
    message 1 4 '    Yes / No    '.
    fast-action
       when ENTER exit.
       when KEND  move ER to last-key exit.
       when NO    move ER to last-key exit.
    loop.

IMPRIME-02000.
;-------------
    message 1 1 '    Conecte     '.
    message 1 2 '  Impresora y   '.
    message 1 3 ' Presione ENTER '.
    message 1 4 '****************'.
    fast-action
       when ENTER connect PRINTER-COM-80
                     when ER move 26 to punt-error
                             perform MUESTRA-ERROR
                             disconnect
                             loop.
                  clear
                  message 2 4 'Imprimiendo...'
                  perform IMPRIME-03000
                  disconnect
                  exit.
       when KEND  exit.
    loop.

IMPRIME-03000.
;-------------
    fill w-prn-zona with ' '.
    move w-numero-factura to w-prn-nro-factura.
    move w-cod-cliente    to w-prn-cliente.
    if punt-venta = 3                       ; cliente nuevo en ingreso
       perform IMPRIME-03100
    else
       if punt-venta = 1                    ; cliente ruta en ingreso
          perform IMPRIME-03200
       else
          if w-es-cliente-nuevo = 1         ; cliente nuevo en correccion
             perform IMPRIME-03100
          else
             perform IMPRIME-03200          ; cliente ruta en correccion
          end-if
       end-if
    end-if
    perform ABRE-REF-CABECERA
       when ER move 25 to punt-error
               perform MUESTRA-ERROR
               exit.
    search FIRST for w-numero-pedido in CABECERA-PEDIDO-F
       when ER move 25 to punt-error
               perform MUESTRA-ERROR
               exit.
    read CABECERA-PEDIDO-F.

    if punt-venta = 1          ; no es modificacion
       if w-pvta-autoventa = 0
          move w-numero-factura to cpe-numero-factura
       else
          move 0                to cpe-numero-factura
       end-if
       rewrite REG-CABECERA-PEDIDO.

    fill w-prn-forma-pago with ' '.
    perform ABRE-REF-CONDICION-DE-PAGO
       when OK search FIRST for cpe-forma-pago in CONDICION-PAGO-F
                  when OK read CONDICION-PAGO-F
                          move cpa-nombre to w-prn-forma-pago.
            end-when
    move w-codigo-vendedor to w-prn-cod-vendedor.
    move w-nombre-vendedor to w-prn-nom-vendedor.
    move cpe-total         to w-prn-neto.
    move cpe-fecha-vencim  to w-fecha-vencimiento.
    perform CALCULA-TOT-PRN.
    perform CALCULA-FECHA.
    build REP-CABECERA. print.
    move 0 to w-lineas-impresas.
    perform IMPRIME-DETALLES.
    perform IMPRIME-BONIFICACIONES.
    perform IMPRIME-LINEAS-FALTANTES.
    ; calcula el descuento financiero.....
    move     w-prn-neto      to   w-prn-decto-financ.
    subtract w-prn-sub-total from w-prn-decto-financ.
    ;
    build REP-TOTALES. print.
    build REP-LINEA. print.               ; Salto de pagina
    build REP-LINEA. print.
    build REP-LINEA. print.
    build REP-LINEA. print.
    build REP-LINEA. print.
    build REP-LINEA. print.
    build REP-LINEA. print.
    build REP-LINEA. print.
    exit.

IMPRIME-03100.
;-------------
    move w-nombre-clinue    to w-prn-nom-cliente.
    move w-direc-clinue     to w-prn-direccion.
    fill w-prn-comuna with ' '.
    move w-cod-ciudad       to w-busca-ciudad.
    perform BUSCA-CIUDAD.
    move w-clinue-rut       to w-tmp-rut.
    move '-'                to w-tmp-guion.
    move w-clinue-digito    to w-tmp-digito.
    move w-tmp-rut-clinuevo to w-prn-rut.
    move w-fono-clinue      to w-prn-telefono.
    exit.

IMPRIME-03200.
;-------------
    move w-nombre-cliente to w-prn-nom-cliente.
    move w-direccion      to w-prn-direccion.
    move cli-comuna       to w-prn-comuna.
    move cli-ciudad       to w-busca-ciudad.
    perform BUSCA-CIUDAD.
    perform BUSCA-ZONA.
    move cli-rut          to w-prn-rut.
    move cli-telefono     to w-prn-telefono.
    exit.

BUSCA-CIUDAD.
;------------
    perform ABRE-REF-CIUDAD
       when ER exit.
    search FIRST for w-busca-ciudad in CIUDAD-F
       when OK read CIUDAD-F
               move ciu-nombre to w-prn-ciudad.
    exit.

BUSCA-ZONA.
;----------
    if cli-zona = 1  move 'I REGION            ' to w-prn-zona.
    if cli-zona = 2  move 'II REGION           ' to w-prn-zona.
    if cli-zona = 3  move 'III REGION          ' to w-prn-zona.
    if cli-zona = 4  move 'IV REGION           ' to w-prn-zona.
    if cli-zona = 5  move 'V REGION            ' to w-prn-zona.
    if cli-zona = 6  move 'VI REGION           ' to w-prn-zona.
    if cli-zona = 7  move 'VII REGION          ' to w-prn-zona.
    if cli-zona = 8  move 'VIII REGION         ' to w-prn-zona.
    if cli-zona = 9  move 'IX REGION           ' to w-prn-zona.
    if cli-zona = 10 move 'X REGION            ' to w-prn-zona.
    if cli-zona = 11 move 'XI REGION           ' to w-prn-zona.
    if cli-zona = 12 move 'XII REGION          ' to w-prn-zona.
    if cli-zona = 13 move 'REGION METROPOLITANA' to w-prn-zona.
    exit.

CALCULA-TOT-PRN.
;---------------
    move cpe-total to w-calculo1.
    move cpe-total to w-calculo2.
    multiply w-calculo1 by cpe-decto-financiero.
    divide 100 into w-calculo1.

    ; aproxima y saca decimales
    add 50 to w-calculo1.
    divide 100 into w-calculo1.

    subtract w-calculo1 from w-calculo2.
    move w-calculo2 to w-prn-sub-total.

    move w-prn-sub-total to w-calculo1.
    move w-prn-sub-total to w-calculo2.
    multiply w-calculo1 by w-iva.

    ; aproxima y saca decimales
    add 50 to w-calculo1.
    divide 100 into w-calculo1.

    add w-calculo1 to w-calculo2.

    move w-calculo1 to w-prn-total-iva.
    move w-calculo2 to w-prn-total-final.
    exit.

CALCULA-FECHA.
;-------------
    move cpe-fecha-vencim to w-fecha.
    if w-fecha-mes = 01 move 'ENE' to w-prn-mes-vencim.
    if w-fecha-mes = 02 move 'FEB' to w-prn-mes-vencim.
    if w-fecha-mes = 03 move 'MAR' to w-prn-mes-vencim.
    if w-fecha-mes = 04 move 'ABR' to w-prn-mes-vencim.
    if w-fecha-mes = 05 move 'MAY' to w-prn-mes-vencim.
    if w-fecha-mes = 06 move 'JUN' to w-prn-mes-vencim.
    if w-fecha-mes = 07 move 'JUL' to w-prn-mes-vencim.
    if w-fecha-mes = 08 move 'AGO' to w-prn-mes-vencim.
    if w-fecha-mes = 09 move 'SEP' to w-prn-mes-vencim.
    if w-fecha-mes = 10 move 'OCT' to w-prn-mes-vencim.
    if w-fecha-mes = 11 move 'NOV' to w-prn-mes-vencim.
    if w-fecha-mes = 12 move 'DIC' to w-prn-mes-vencim.
    move w-fecha-dia to w-prn-dia-vencim.
    move w-fecha-ano to w-prn-ano-vencim.

CALCULA-FECHA-VENCIMIENTO.
;-------------------------
    move cpe-fecha to w-fecha.
    if w-fecha-mes = 01 move 'ENE' to w-prn-mes.
    if w-fecha-mes = 02 move 'FEB' to w-prn-mes.
    if w-fecha-mes = 03 move 'MAR' to w-prn-mes.
    if w-fecha-mes = 04 move 'ABR' to w-prn-mes.
    if w-fecha-mes = 05 move 'MAY' to w-prn-mes.
    if w-fecha-mes = 06 move 'JUN' to w-prn-mes.
    if w-fecha-mes = 07 move 'JUL' to w-prn-mes.
    if w-fecha-mes = 08 move 'AGO' to w-prn-mes.
    if w-fecha-mes = 09 move 'SEP' to w-prn-mes.
    if w-fecha-mes = 10 move 'OCT' to w-prn-mes.
    if w-fecha-mes = 11 move 'NOV' to w-prn-mes.
    if w-fecha-mes = 12 move 'DIC' to w-prn-mes.
    move w-fecha-dia to w-prn-dia.
    move w-fecha-ano to w-prn-ano.
    exit.

IMPRIME-DETALLES.
;----------------
    build REP-LINEA. print.
    build REP-LINEA. print.
    perform ABRE-REF-PRODUCTOS
       when ER exit.
    move cpe-lista-precio to w-lista-precio.
    perform ABRE-REF-LISTA-PRECIO
       when ER exit.
    perform ABRE-REF-DETALLE-PEDIDO
       when ER exit.
    search FIRST for w-numero-pedido in DETALLE-PEDIDO-F
       when ER exit.
    read DETALLE-PEDIDO-F.

IMPRIME-DET-0100.
;----------------
    if dpe-numero-pedido not = w-numero-pedido exit.
    fill w-prn-desc-producto with ' '.
    fill w-prn-unidad-medida with ' '.
    fill w-prn-deta-precio   with ' '.
    move dpe-producto   to w-prn-cod-producto.
    move dpe-descuento  to w-prn-desc-detalle.
    move dpe-cantidad   to w-prn-cant-detalle.
    move dpe-total-item to w-prn-total-detalle.
    search FIRST for dpe-producto in PRODUCTOS-F
       when OK read PRODUCTOS-F
               move pro-descripcion   to w-prn-desc-producto
               move pro-unidad-medida to w-prn-unidad-medida.
    search FIRST for dpe-producto in LISTA-PRECIO-F
       when OK read LISTA-PRECIO-F
               move lpr-precio to w-prn-deta-precio.

    build REP-DETALLE. print.
    add 1 to w-lineas-impresas.
    seek NEXT in DETALLE-PEDIDO-F
       when EOF exit.
    read DETALLE-PEDIDO-F.
    loop.

IMPRIME-BONIFICACIONES.
;----------------------
    perform ABRE-REF-PRODUCTOS
       when ER exit.
    perform ABRE-REF-BONIFICACION
       when ER exit.
    search FIRST for w-numero-pedido in BONIFICACION-F
       when ER exit.
    read BONIFICACION-F.

IMPRIME-BONI-0100.
;-----------------
    if bon-numero-pedido not = w-numero-pedido exit.
    fill w-prn-desc-producto with ' '.
    fill w-prn-unidad-medida with ' '.
    move bon-producto to w-prn-cod-producto.
    move bon-cantidad to w-prn-cant-detalle.
    search FIRST for bon-producto in PRODUCTOS-F
       when OK read PRODUCTOS-F
               move pro-descripcion   to w-prn-desc-producto
               move pro-unidad-medida to w-prn-unidad-medida.

    build REP-BONIFICACION. print.
    add 1 to w-lineas-impresas.
    seek NEXT in BONIFICACION-F
       when EOF exit.
    read BONIFICACION-F.
    loop.

IMPRIME-LINEAS-FALTANTES.
;------------------------
    move w-maximo-itemes to w-contabiliza-lineas.
    add 1 to w-contabiliza-lineas.
    subtract w-lineas-impresas from w-contabiliza-lineas.
    perform IMPRIME-LINEAS until w-contabiliza-lineas < 0.
    exit.

IMPRIME-LINEAS.
;--------------
    build REP-LINEA. print.
    subtract 1 from w-contabiliza-lineas.
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                           CORRECCION DEL PEDIDO                            ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
CORRECCION.
;----------
    perform ABRE-REF-CLIENTES
       when ER exit.

    move 0 to w-existe-clinuevo.
    perform ABRE-REF-CLTE-NUEVO
       when ER move 1 to w-existe-clinuevo.

    perform ABRE-UP-BONIFICACION.
    perform ABRE-REF-BONIFICACION.

    move 0 to w-existe-condicion-pago.
    move 0 to w-dias-vencimiento.
    fill w-cond-pago-efectivo with ' '.
    perform ABRE-REF-CONDICION-DE-PAGO
       when ER move 1 to w-existe-condicion-pago.
    seek FIRST in CONDICION-PAGO-F
       when EOF move 1 to w-existe-condicion-pago.
       when OK  read CONDICION-PAGO-F
                move cpa-codigo to w-cond-pago-efectivo
                move cpa-dias   to w-dias-vencimiento.

CORRECCION-01000.
;----------------
    perform ABRE-REF-CABECERA
       when ER move 27 to punt-error
               perform MUESTRA-ERROR
               exit.
    seek LAST in CABECERA-PEDIDO-F
       when EOF move 27 to punt-error
                perform MUESTRA-ERROR
                exit.
    read CABECERA-PEDIDO-F.

    perform ABRE-REF-DETALLE-PEDIDO
       when ER exit.

    perform CORRECCION-01100
       when OK loop.
    exit.

CORRECCION-01100.
;----------------
    fill w-nombre-cliente with ' '.
    move 0 to w-es-cliente-nuevo.
    move cpe-numero-pedido  to w-numero-pedido.
    move cpe-numero-factura to w-numero-factura.
    search FIRST for cpe-cliente in CLIENTES-F
       when OK read CLIENTES-F
               move cli-nombre    to w-nombre-cliente
               move cli-direccion to w-direccion.
       when ER if w-existe-clinuevo = 0
                  search FIRST for cpe-cliente in CLIENTE-NUEVO-F
                     when OK read CLIENTE-NUEVO-F
                             move cln-nombre    to w-nombre-cliente
                             move cln-nombre    to w-nombre-clinue
                             move cln-direccion to w-direc-clinue
                             move cln-ciudad    to w-cod-ciudad
                             move cln-rut       to w-clinue-rut
                             move cln-digito    to w-clinue-digito
                             move cln-telefono  to w-fono-clinue
                             move 1             to w-es-cliente-nuevo.
               end-if
            end-when
    show PEDIDO-A-CORREGIR-SD.
    fast-action
       when KEND  exit.
       when ENTER perform CORRIGE-02000
                     when EOF exit.
            end-when
       when UAR   show SCROLL-CLI-REVISA-PED-SD
                  wait 7 tenths.
       when FAR   seek NEXT in CABECERA-PEDIDO-F
                     when EOF seek LAST in CABECERA-PEDIDO-F
                              beep.
                  read CABECERA-PEDIDO-F.
       when BAR   seek PRIOR in CABECERA-PEDIDO-F
                     when EOF seek FIRST in CABECERA-PEDIDO-F
                              beep.
                  read CABECERA-PEDIDO-F.
       when KDEL  perform SEGURO-BORRA-PEDIDO
                     when KEND loop.
                  move OK to last-key
                  exit.
    loop.

SEGURO-BORRA-PEDIDO.
;-------------------
    beep.
    message 1 1 '****************'.
    message 1 2 ' Seguro Elimina '.
    message 1 3 '    Yes / No    '.
    message 1 4 '****************'.
    fast-action
       when ENTER next.
       when NO    move KEND to last-key exit.
       when KEND  exit.
    loop.

BORRA-PEDIDO-0100.
;-----------------
    message 1 1 '****************'.
    message 1 2 ' Presione ERASE '.
    message 1 3 '   Por 3 Beep   '.
    message 1 4 '****************'.
    fast-action
       when KDEL 3-beep
                    when OK next.
            end-when
    move KEND to last-key.
    exit.

BORRA-PEDIDO-0200.
;-----------------
    message 1 1 '****************'.
    message 1 2 '    Espere...   '.
    message 1 3 'Eliminando Venta'.
    message 1 4 '****************'.
    perform ABRE-UP-CABECERA-PEDIDO
    search FIRST for w-numero-pedido in CABECERA-PEDIDO-F
       when ER exit.
    read CABECERA-PEDIDO-F.
    if w-es-cliente-nuevo = 0
       if w-existe-condicion-pago = 0
          if cpe-forma-pago not = w-cond-pago-efectivo
             subtract cpe-total from cli-deuda-pendiente
             rewrite REG-CLIENTES.
       end-if
    end-if
    move cpe-bodega to w-revisa-bodega.
    perform ABRE-REF-2PRODUCTOS
       when ER exit.
    delete RECORD from CABECERA-PEDIDO-F.
    perform ABRE-UP-DETALLE-PEDIDO.
    perform BORRA-PEDIDO-0300.

    perform ABRE-UP-BONIFICACION
       when ER exit.
    perform BORRA-PEDIDO-0400.

    perform ABRE-UP-PAGOS-VENTA
       when ER exit.
    perform BORRA-PEDIDO-0500.

    perform ABRE-UP-OBSERVACIONES.
    search FIRST for w-numero-pedido in OBSERVACIONES-F
       when ER exit.
    read OBSERVACIONES-F.
    delete RECORD from OBSERVACIONES-F.
    exit.

BORRA-PEDIDO-0300.          ; Borra detalles del pedido
;-----------------
    search FIRST for w-numero-pedido in DETALLE-PEDIDO-F
       when ER exit.
    read DETALLE-PEDIDO-F.
    search FIRST for dpe-producto in PRODUCTOS-F
       when OK read PRODUCTOS-F
               add dpe-cantidad to pro-stock
               rewrite REG-PRODUCTOS.
    delete RECORD from DETALLE-PEDIDO-F.
    loop.

BORRA-PEDIDO-0400.          ; Borra bonificaciones del pedido
;-----------------
    search FIRST for w-numero-pedido in BONIFICACION-F
       when ER exit.
    read BONIFICACION-F.
    search FIRST for dpe-producto in PRODUCTOS-F
       when OK read PRODUCTOS-F
               add bon-cantidad to pro-stock
               rewrite REG-PRODUCTOS.
    delete RECORD from BONIFICACION-F.
    loop.

BORRA-PEDIDO-0500.          ; Borra pagos de la venta
;-----------------
    search FIRST for w-numero-pedido in PAGO-VENTA-F
       when ER exit.
    read PAGO-VENTA-F.
    delete RECORD from PAGO-VENTA-F.
    loop.

; correccion de la nota de pedido
CORRIGE-02000.
;-------------
    move 0 to w-agrega-item.
    move 0 to w-credito-venta.
    if w-es-cliente-nuevo = 0               ; no es cliente nuevo
       move cli-linea-credito to w-credito-venta
       multiply w-credito-venta by 105      ; 1.05 (para sumarle 5% de holgura)
       divide 100 into w-credito-venta
       subtract cli-deuda-pendiente from w-credito-venta.

    move cpe-bodega to w-revisa-bodega.
    perform ABRE-REF-2PRODUCTOS
       when ER exit.
    move cpe-lista-precio to w-lista-precio.
    perform ABRE-REF-LISTA-PRECIO
       when ER exit.

    search FIRST for w-numero-pedido in DETALLE-PEDIDO-F
       when ER move 28 to punt-error
               perform MUESTRA-ERROR
               exit.
    read DETALLE-PEDIDO-F.
    move 1 to w-cuenta-itemes.
    perform CORRIGE-02100.
    if w-agrega-item = 1 loop.
    exit.

CORRIGE-02100.
;-------------
    seek NEXT in DETALLE-PEDIDO-F
       when EOF next.
    read DETALLE-PEDIDO-F.
    if dpe-numero-pedido not = w-numero-pedido next.
    add 1 to w-cuenta-itemes.
    loop.

CORRIGE-02200.
;-------------
    search FIRST for w-numero-pedido in DETALLE-PEDIDO-F.
    read DETALLE-PEDIDO-F.
    move 0 to w-pantalla.
    beep.

    search FIRST for w-numero-pedido in BONIFICACION-F
       when OK read BONIFICACION-F
               add 1 to w-cuenta-itemes
               perform CORRIGE-02250.
    perform CORRIGE-02300.
    exit.

CORRIGE-02250.
;-------------
    seek NEXT in BONIFICACION-F
       when EOF exit.
    read BONIFICACION-F.
    if bon-numero-pedido not = w-numero-pedido exit.
    add 1 to w-cuenta-itemes.
    loop.

CORRIGE-02300.
;-------------
    fill w-cod-prod with '*'.
    move 0 to w-precio.
    move 0 to w-bonificacion.
    move 1 to punt-corrige.
    move 0 to w-existe-producto.
    move 0 to w-existe-lis-pre.
    move 0 to w-hay-bonificacion.
    move 0 to w-agrega-item.
    search FIRST for dpe-producto in PRODUCTOS-F
       when OK read PRODUCTOS-F
               move pro-descripcion to w-cod-prod
               move 1               to w-existe-producto.
    search FIRST for dpe-producto in LISTA-PRECIO-F
       when OK read LISTA-PRECIO-F
               move lpr-precio to w-precio
               move 1          to w-existe-lis-pre.

    move dpe-numero-pedido to w-llave-bon1.
    move dpe-producto      to w-llave-bon2.
    search FIRST for w-llave-bonificacion in BONIFICACION-F
       when OK read BONIFICACION-F
               move bon-cantidad to w-bonificacion
               move 1            to w-hay-bonificacion.

    move dpe-cantidad  to w-cantidad.
    move dpe-descuento to w-descuento.
    show CORRIGE-ITEM-SD.
    if w-pantalla = 0
       show PANTALLA-01-SD
    else
       show PANTALLA-02-SD.
    fast-action
       when KEND  perform CORRIGE-10000
                  exit.
       when KNEXT if w-pantalla = 0
                     move 1 to w-pantalla
                  else
                     move 0 to w-pantalla.
            end-when
       when ENTER perform MENU-CORRIGE
                     when EOF exit.
                     if w-agrega-item = 1 exit.
            end-when
       when UAR   show CORRIGE-ITEM-SCROLL-SD
                  wait 7 tenths.
       when FAR   perform SGTE-DETALLE.
       when BAR   perform ANTE-DETALLE.
    loop.

SGTE-DETALLE.
;------------
    seek NEXT in DETALLE-PEDIDO-F
       when EOF seek LAST in DETALLE-PEDIDO-F
                beep.
    read DETALLE-PEDIDO-F.
    if dpe-numero-pedido not = w-numero-pedido
       beep
       seek PRIOR in DETALLE-PEDIDO-F
       read DETALLE-PEDIDO-F.
    exit.

ANTE-DETALLE.
;------------
    seek PRIOR in DETALLE-PEDIDO-F
       when EOF seek FIRST in DETALLE-PEDIDO-F
                beep.
    read DETALLE-PEDIDO-F.
    if dpe-numero-pedido not = w-numero-pedido
       beep
       seek NEXT in DETALLE-PEDIDO-F
       read DETALLE-PEDIDO-F.
    exit.

MENU-CORRIGE.
;------------
    message 1 1 'CORRIGE DATOS   '.
    if punt-corrige < 4
       message 1 2 '1 Cantidad      '
       message 1 3 '2 Descuento     '
       message 1 4 '3 Bonificacion  '
    else
       message 1 2 '4 Anula Item    '
       message 1 3 '5 Agrega Item   '
       message 1 4 '                '.

    examine punt-corrige
       when 0 move 5 to punt-corrige loop.
       when 1 message 2 2, '>'.
       when 2 message 2 3, '>'.
       when 3 message 2 4, '>'.
       when 4 message 2 2, '>'.
       when 5 message 2 3, '>'.
       when 6 move 1 to punt-corrige loop.
    fast-action
       when ENTER perform MENU-CORRIGE-01000 exit.
       when DIG   if data-key > 0 and data-key < 6
                     move data-key to punt-corrige
                     perform MENU-CORRIGE-01000
                     exit
                  end-if
            end-when
       when FAR   add      1 to   punt-corrige.
       when BAR   subtract 1 from punt-corrige.
       when KEND  exit.
    loop.

MENU-CORRIGE-01000.
;------------------
    examine punt-corrige
       when 1 perform CORRIGE-03000.       ; cambia cantidad
       when 2 perform CORRIGE-04000.       ; cambia descuento
       when 3 perform CORRIGE-05000.       ; cambia bonificacion
       when 4 perform CORRIGE-06000.       ; anula item
       when 5 perform CORRIGE-07000.       ; agrega item
    exit.

CORRIGE-03000.           ; cambia cantidad
;-------------
    if w-existe-producto = 0
       move 29 to punt-error
       perform MUESTRA-ERROR
       exit.
    if w-existe-lis-pre = 0
       move 30 to punt-error
       perform MUESTRA-ERROR
       exit.
    move 0 to w-cantidad.
    show CORRIGE-ITEM-SD.
    show PANTALLA-02-SD.
    accept MODI-CANTIDAD-SD
       when OK   if w-cantidad = 0 loop.
                 move pro-stock to w-stock
                 add dpe-cantidad to w-stock
                 if w-cantidad > w-stock
                    beep
                    show FALTA-STOCK-SD
                    wait 1 second
                    if w-pvta-autoventa = 0           ; autoventa
                       loop.
                 end-if
                 perform CALCULA-SUB-TOTAL
                 perform CORRIGE-03100
                    when ER exit.
                 ; rebaja stock y regraba cantidad
                 add dpe-cantidad to pro-stock
                 rewrite REG-PRODUCTOS
                 subtract w-cantidad from pro-stock
                 rewrite REG-PRODUCTOS
                 move w-cantidad   to dpe-cantidad
                 move w-total-item to dpe-total-item
                 rewrite REG-DETALLE-PEDIDO
                 exit.
       when KEND exit.
    loop.

CORRIGE-03100.
;-------------
    if w-cod-cond-pago not = w-cond-pago-efectivo
       if w-es-cliente-nuevo = 0
          add      dpe-total-item to   w-credito-venta
          subtract w-total-item   from w-credito-venta
          if w-credito-venta <= 0 and w-pvta-autoventa not = 0 ;no es autoventa
             subtract dpe-total-item from w-credito-venta
             add      w-total-item   to   w-credito-venta
             move 21 to punt-error
             perform MUESTRA-ERROR
             exit.
          subtract dpe-total-item from cli-deuda-pendiente
          add      w-total-item   to   cli-deuda-pendiente
          rewrite REG-CLIENTES.
    end-if
    if w-existe-lis-pre = 1
       if w-cantidad < lpr-cant-bonifica              ; no hay bonificacion
          if w-hay-bonificacion = 1
             perform ABRE-UP-BONIFICACION
             search FIRST for w-llave-bonificacion in BONIFICACION-F
                when OK read BONIFICACION-F
                        add bon-cantidad to pro-stock
                        rewrite REG-PRODUCTOS
                        delete record from BONIFICACION-F
                        perform ABRE-REF-BONIFICACION
                        if w-pvta-autoventa = 0           ; autoventa
                           subtract 1 from w-cuenta-itemes.
                     end-when
          end-if
       end-if
    end-if
    exit.

CORRIGE-04000.           ; cambia descuento
;-------------
    if w-existe-producto = 0
       move 29 to punt-error
       perform MUESTRA-ERROR
       exit.
    if w-existe-lis-pre = 0
       move 30 to punt-error
       perform MUESTRA-ERROR
       exit.
    move 0 to w-descuento.
    show CORRIGE-ITEM-SD.
    show TOPE-2DESCUENTO-SD.
    accept MODI-DESCUENTO-SD
       when OK   if w-descuento > lpr-descuento
                    beep
                    loop.
                 perform CALCULA-SUB-TOTAL
                 perform CORRIGE-03100
                    when ER exit.
                 move w-descuento  to dpe-descuento
                 move w-total-item to dpe-total-item
                 rewrite REG-DETALLE-PEDIDO
                 exit.
       when KEND exit.
    loop.

CORRIGE-05000.           ; cambia bonificacion
;-------------
    if w-existe-producto = 0
       move 29 to punt-error
       perform MUESTRA-ERROR
       exit.
    if w-existe-lis-pre = 0
       move 30 to punt-error
       perform MUESTRA-ERROR
       exit.
    if dpe-cantidad < lpr-cant-bonifica      ; no hay derecho a bonificacion
       exit.
    if w-hay-bonificacion = 0         ; no habia bonif. antes de este modulo
       if w-pvta-autoventa = 0                    ; autoventa
          if w-cuenta-itemes >= w-maximo-itemes
             move 24 to punt-error
             perform MUESTRA-ERROR
             exit
          end-if
       end-if
    end-if
    move w-cantidad to w-tope-bonificacion.
    divide lpr-cant-bonifica into w-tope-bonificacion.
    multiply w-tope-bonificacion by lpr-bonificacion.

CORRIGE-05100.
;-------------
    move 0 to w-bonificacion.
    show CORRIGE-ITEM-SD.
    show PANTALLA-02-SD.
    accept MODI-BONIFICACION-SD
       when OK   if w-bonificacion = 0 loop.
                 if w-bonificacion > w-tope-bonificacion
                    beep
                    loop.
                 move pro-stock to w-stock
                 if w-hay-bonificacion = 1          ; ya existia bonificacion
                    add bon-cantidad to w-stock.
                 if w-bonificacion > w-stock
                    beep
                    show FALTA-STOCK-SD
                    wait 1 second
                    if w-pvta-autoventa = 0           ; autoventa
                       loop.
                 end-if
                 perform ABRE-UP-BONIFICACION
                    when ER exit.
                 ; rebaja stock y regraba cantidad
                 perform CORRIGE-05200
                 exit.
       when KEND exit.
    loop.

CORRIGE-05200.
;-------------
    if w-hay-bonificacion = 0
       search FIRST for w-numero-pedido in BONIFICACION-F
          when OK read BONIFICACION-F
                  move w-numero-pedido to bon-numero-pedido
                  move pro-codigo      to bon-producto
                  move w-bonificacion  to bon-cantidad
                  move x'0d'           to bon-cr
                  move x'0a'           to bon-lf
                  insert REG-BONIFICACION.
          when ER move w-numero-pedido to bon-numero-pedido
                  move pro-codigo      to bon-producto
                  move w-bonificacion  to bon-cantidad
                  move x'0d'           to bon-cr
                  move x'0a'           to bon-lf
                  write REG-BONIFICACION.
       add 1 to w-cuenta-itemes
       subtract w-bonificacion from pro-stock
       rewrite REG-PRODUCTOS
    else
       search FIRST for w-llave-bonificacion in BONIFICACION-F
       read BONIFICACION-F
       add bon-cantidad to pro-stock
       rewrite REG-PRODUCTOS
       subtract w-bonificacion from pro-stock
       rewrite REG-PRODUCTOS
       move w-bonificacion to bon-cantidad
       rewrite REG-BONIFICACION.

    perform ABRE-REF-BONIFICACION       ; reabre el archivo de referencia
    exit.                               ; para que vuelva a ser utilizado

CORRIGE-06000.
;-------------
    beep.
    message 1 1 '****************'.
    message 1 2 ' Seguro Elimina '.
    message 1 3 '    Yes / No    '.
    message 1 4 '****************'.
    fast-action
       when ENTER next.
       when NO    exit.
       when KEND  exit.
    loop.

CORRIGE-06100.
;-------------
    message 1 1 '****************'.
    message 1 2 ' Presione ERASE '.
    message 1 3 '   Por 3 Beep   '.
    message 1 4 '****************'.
    fast-action
       when KDEL 3-beep
                    when OK next.
            end-when
    exit.

CORRIGE-06200.
;-------------
    if w-es-cliente-nuevo = 0 and w-cod-cond-pago not = w-cond-pago-efectivo
       subtract dpe-total-item from cli-deuda-pendiente
       subtract dpe-total-item from w-credito-venta
       rewrite REG-CLIENTES.

    add dpe-cantidad to pro-stock.
    rewrite REG-PRODUCTOS.
    if w-hay-bonificacion = 1
       add w-bonificacion to pro-stock
       rewrite REG-PRODUCTOS.

    subtract 1 from w-cuenta-itemes.
    if w-hay-bonificacion = 1
       subtract 1 from w-cuenta-itemes.

    perform ABRE-UP-BONIFICACION
    search FIRST for w-llave-bonificacion in BONIFICACION-F
       when OK read BONIFICACION-F
               delete RECORD from BONIFICACION-F.
    perform ABRE-REF-BONIFICACION.

    perform ABRE-UP-DETALLE-PEDIDO
    search FIRST for w-llave-bonificacion in DETALLE-PEDIDO-F
       when OK read DETALLE-PEDIDO-F
               delete RECORD from DETALLE-PEDIDO-F.

    perform ABRE-REF-DETALLE-PEDIDO.
    search FIRST for w-numero-pedido in DETALLE-PEDIDO-F
       when ER perform ABRE-UP-OBSERVACIONES
               search FIRST for w-numero-pedido in OBSERVACIONES-F
                  when OK read OBSERVACIONES-F
                          delete RECORD from OBSERVACIONES-F.
               perform ABRE-UP-CABECERA-PEDIDO
               search FIRST for w-numero-pedido in CABECERA-PEDIDO-F
                  when OK read CABECERA-PEDIDO-F
                          delete RECORD from CABECERA-PEDIDO-F.
               perform ABRE-REF-CABECERA-PEDIDO
               move 31 to punt-error
               perform MUESTRA-ERROR
               move EOF to last-key
               exit.
    read DETALLE-PEDIDO-F.
    exit.

CORRIGE-07000.                 ; agrega item
;-------------
    move 32 to punt-error.
    perform MUESTRA-ERROR.

    move 1 to w-agrega-item.
    perform VENTA-01000.

    perform ABRE-REF-DETALLE-PEDIDO.
    search FIRST for w-numero-pedido in DETALLE-PEDIDO-F.
    read DETALLE-PEDIDO-F.

    move 34 to punt-error.
    perform MUESTRA-ERROR.

    perform ABRE-REF-BONIFICACION.
    perform ABRE-REF-CABECERA
;    perform ABRE-REF-DETALLE-PEDIDO            ojo, esta linea esta de mas

    exit.

CORRIGE-10000.                     ; finaliza revision
;-------------
    move 1                    to w-ultima-condicion.
    move 0                    to w-total-venta.
    move cpe-decto-financiero to w-descuento.
    search FIRST for w-numero-pedido in DETALLE-PEDIDO-F.
    read DETALLE-PEDIDO-F.
    perform CORRIGE-10100.           ; para saber solamente el w-total-venta
    perform CORRIGE-10200.
    exit.

CORRIGE-10100.
;-------------
    if w-numero-pedido not = dpe-numero-pedido exit.
    add dpe-total-item to w-total-venta.
    seek NEXT in DETALLE-PEDIDO-F
       when EOF exit.
    read DETALLE-PEDIDO-F.
    loop.

CORRIGE-10200.
;-------------
    move cpe-forma-pago to w-cod-cond-pago.
    if w-existe-condicion-pago = 0              ; existe archivo forma de pago
       search FIRST for w-cod-cond-pago in CONDICION-PAGO-F
          when ER move 1 to w-existe-condicion-pago
                  loop.
       read CONDICION-PAGO-F
       perform CAMBIA-FORMA-PAGO
       move cpa-decto-financiero to w-decto-financiero
       move cpa-codigo           to w-cod-cond-pago
       move cpa-dias             to w-dias-vencimiento.


    perform CALCULA-DIA-VENCIMIENTO.
    if w-cod-cond-pago not = w-cond-pago-efectivo
       perform CORRIGE-10300.

    perform CALCULA-TOTALES-FINALES.
    perform MUESTRA-TOTAL-FINAL.
    perform MUESTRA-OBSERVACON

    perform GRABA-TOTALES-FINALES.
    if w-pvta-autoventa = 0
       perform IMPRIME-FACTURA.
    perform LEE-PARAMETROS.
    if w-cod-cond-pago = w-cond-pago-efectivo
       perform ABRE-UP-PAGOS-VENTA
       perform CORRIGE-10250.
    exit.

MUESTRA-OBSERVACON.
;------------------
    perform ABRE-REF-OBSERVACIONES
       when ER perform INGRESA-OBSERVACION
               exit.
    search FIRST for w-numero-pedido in OBSERVACIONES-F
       when ER perform INGRESA-OBSERVACIONES
               exit.
    read OBSERVACIONES-F.
    move obs-observacion to w-observacion.

MUESTRA-OBS-0100.
;----------------
    show OBSERVACION-SD.
    fast-action
       when KEND  exit.
       when ENTER exit.
       when CLR   perform INGRESA-OBSERVACION
                     when KEND move obs-observacion to w-observacion
                               loop.
                  exit.
    loop.

CORRIGE-10250.
;-------------
    search FIRST for w-numero-pedido in PAGO-VENTA-F
       when ER exit.
    read PAGO-VENTA-F.                        ; Borra todos los anteriores
    delete record from PAGO-VENTA-F.          ; documentos ya que cambio la
    loop.                                     ; condicion de "?" a "efectivo"

CORRIGE-10300.
;-------------
    move 0 to w-existe-banco.
    move 0 to w-existe-plaza.
    perform ABRE-REF-BANCO
       when ER move 1 to w-existe-banco.
    seek FIRST in BANCO-F
       when EOF move 1 to w-existe-banco.

    perform ABRE-REF-PLAZA
       when ER move 1 to w-existe-plaza.
    seek FIRST in PLAZA-F
       when EOF move 1 to w-existe-plaza.

    if w-es-cliente-nuevo = 1 exit.            ; cliente nuevo
    move 1 to punt-pagos.
    perform ABRE-UP-PAGOS-VENTA
       when MEM exit.
    search FIRST for w-numero-pedido in PAGO-VENTA-F
       when OK read PAGO-VENTA-F
               perform CORRIGE-PAGOS
                  when EOF loop.
            end-when
       when ER perform INGRESA-DOCUMENTOS.
    exit.

CORRIGE-PAGOS.
;-------------
    move 1 to punt-pagos.
    move 1 to punt-corrige.
    show REVISA-PAGOS-SD.
    fast-action
       when ENTER perform MENU-PAGO-CORRIGE
                     when EOF exit.
            end-when
       when FAR   perform SGTE-CORRIGE-PAGO.
       when BAR   perform ANTE-CORRIGE-PAGO.
       when KEND  exit.
       when KNEXT if w-existe-banco = 1 or w-existe-plaza = 1
                     exit.
                  search FIRST for pon-banco in BANCO-F
                     when ER loop.
                  read BANCO-F
                  search FIRST for pon-plaza in PLAZA-F
                     when ER loop.
                  read PLAZA-F
                  beep
                  show BANCO-SD
                  show PLAZA-SD
                  action.
    loop.

SGTE-CORRIGE-PAGO.
;-----------------
    seek NEXT in PAGO-VENTA-F
       when EOF beep
                seek LAST in PAGO-VENTA-F.
    read PAGO-VENTA-F.
    if pon-numero-pedido not = w-numero-pedido
       beep
       seek PRIOR in PAGO-VENTA-F
       read PAGO-VENTA-F.
    exit.

ANTE-CORRIGE-PAGO.
;-----------------
    seek PRIOR in PAGO-VENTA-F
       when EOF beep
                seek FIRST in PAGO-VENTA-F.
    read PAGO-VENTA-F.
    if pon-numero-pedido not = w-numero-pedido
       beep
       seek NEXT in PAGO-VENTA-F
       read PAGO-VENTA-F.
    exit.

MENU-PAGO-CORRIGE.
;-----------------
    message 1 1 'CORRIGE PAGOS   '.
    if punt-corrige < 4
       message 1 2 '1 Nro.Documento '
       message 1 3 '2 Nro. Serie    '
       message 1 4 '3 Cta. Corriente'
    else
       if punt-corrige > 3 and punt-corrige < 7
          message 1 2 '4 Monto         '
          message 1 3 '5 Banco         '
          message 1 4 '6 Plaza         '
       else
          message 1 2 '7 Agrega Pago   '
          message 1 3 '8 Elimina Pago  '
          message 1 4 '                '.
    end-if

    examine punt-corrige
       when 0 move 8 to punt-corrige loop.
       when 1 message 2 2, '>'.
       when 2 message 2 3, '>'.
       when 3 message 2 4, '>'.
       when 4 message 2 2, '>'.
       when 5 message 2 3, '>'.
       when 6 message 2 4, '>'.
       when 7 message 2 2, '>'.
       when 8 message 2 3, '>'.
       when 9 move 1 to punt-corrige loop.
    fast-action
       when ENTER perform MENU-PAGO-01000 exit.
       when DIG   if data-key > 0 and data-key < 9
                     move data-key to punt-corrige
                     perform MENU-PAGO-01000
                     exit
                  end-if
            end-when
       when FAR   add      1 to   punt-corrige.
       when BAR   subtract 1 from punt-corrige.
       when KEND  exit.
    loop.

MENU-PAGO-01000.
;---------------
    examine punt-corrige
       when 1 perform CORRIGE-DOCUM.
       when 2 perform CORRIGE-SERIE.
       when 3 perform CORRIGE-CTACTE.
       when 4 perform CORRIGE-MONTO.
       when 5 if w-existe-banco = 1 exit.
              perform ACEPTA-BANCO-0100
                 when OK move w-cod-banco to pon-banco
                         rewrite REG-PAGO-VENTA.
            end-when
       when 6 if w-existe-plaza = 1 exit.
              perform ACEPTA-PLAZA-0100
                 when OK move w-cod-plaza to pon-plaza
                         rewrite REG-PAGO-VENTA.
            end-when
       when 7 perform AGREGA-PAGO.
       when 8 perform ELIMINA-PAGOS.
    exit.

CORRIGE-DOCUM.
;-------------
    move 0 to w-nro-docum.
    accept NRO-DOCUM-SD.
       when OK   if entry-size = 0 loop.
                 move w-nro-docum to pon-documento
                 rewrite REG-PAGO-VENTA
                 exit.
       when KEND exit.
    loop.

CORRIGE-SERIE.
;-------------
    fill w-nro-serie with ' '.
    show NRO-DOCUM-SD.
    accept NRO-SERIE-SD.
       when OK   if entry-size = 0 loop.
                 move w-nro-serie to pon-serie
                 rewrite REG-PAGO-VENTA
                 exit.
       when KEND exit.
    loop.

CORRIGE-CTACTE.
;--------------
    move 0 to w-cta-cte.
    show NRO-DOCUM-SD.
    show NRO-SERIE-SD.
    accept CTA-CTE-SD.
       when OK   if entry-size = 0 loop.
                 move w-cta-cte to pon-cuenta-cte
                 rewrite REG-PAGO-VENTA
                 exit.
       when KEND exit.
    loop.

CORRIGE-MONTO.
;-------------
    move 0 to w-monto-pago.
    clear.
    accept MONTO-PAGO-SD
       when OK   if entry-size = 0 loop.
                 move w-monto-pago to pon-monto
                 rewrite REG-PAGO-VENTA
                 exit.
       when KEND exit.
    loop.

AGREGA-PAGO.
;-----------
    examine punt-pagos
       when 0 move 1 to punt-pagos
              move 0 to w-nro-docum
              move 0 to w-cta-cte
              move 0 to w-monto-pago
              fill w-cod-banco with ' '
              fill w-cod-plaza with ' '
              fill w-nro-serie with ' '
              loop.
       when 1 accept NRO-DOCUM-SD.
       when 2 show NRO-DOCUM-SD
              accept NRO-SERIE-SD.
       when 3 show NRO-DOCUM-SD
              show NRO-SERIE-SD
              accept CTA-CTE-SD.
       when 4 perform ACEPTA-BANCO.
       when 5 perform ACEPTA-PLAZA.
       when 6 clear
              accept MONTO-PAGO-SD.
       when 7 beep
              move w-numero-pedido to pon-numero-pedido
              move w-nro-docum     to pon-documento
              move w-nro-serie     to pon-serie
              move w-cta-cte       to pon-cuenta-cte
              move w-cod-banco     to pon-banco
              move w-cod-plaza     to pon-plaza
              move w-monto-pago    to pon-monto
              move x'0d'           to pon-cr
              move x'0a'           to pon-lf
              insert REG-PAGO-VENTA
                 when MEM perform MEMORIA-LLENA.
              move OK to last-key.
       when 8 move 1 to punt-pagos loop.
    no-op
       when OK   add 1 to punt-pagos.
       when KEND if punt-pagos = 1
                    exit.
                 subtract 1 from punt-pagos.
       when BAR  subtract 1 from punt-pagos.
    loop.

ELIMINA-PAGOS.
;-------------
    beep.
    message 1 1 '****************'.
    message 1 2 ' Seguro Elimina '.
    message 1 3 '    Yes / No    '.
    message 1 4 '****************'.
    fast-action
       when ENTER next.
       when NO    exit.
       when KEND  exit.
    loop.

ELIMINA-PAGOS-0100.
;-----------------
    message 1 1 '****************'.
    message 1 2 ' Presione ERASE '.
    message 1 3 '   Por 3 Beep   '.
    message 1 4 '****************'.
    fast-action
       when KDEL 3-beep
                    when OK next.
            end-when
    exit.

ELIMINA-PAGOS-0200.
;-----------------
    delete RECORD from PAGO-VENTA-F.
    search FIRST for w-numero-pedido in PAGO-VENTA-F
       when ER move EOF to last-key
               exit.
    read PAGO-VENTA-F.
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                              MUESTRA ERRORES                               ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
MUESTRA-ERROR.
;-------------
    alarm
    examine punt-error
       when 1  message 1 1 '   No Existe      Programa de    Comunicaciones'.
       when 2  message 2 4 'Fecha No Valida'.
       when 3  message 1 3 '** No Existen **** Parametros **'.
       when 4  message 1 3 'No Hay  Familias  para Trabajar '.
       when 5  message 1 3 'No Hay SubFamil.  para Trabajar '.
       when 6  clear, message 1 1 '  No Hay Datos     de Ningun        Cliente'.
       when 7  clear, message 1 1 '  No Hay Datos     de Ningun        Producto'.
       when 8  message 1 2 '  No hay Datos    de Listas de       Precio     '.
       when 9  message 1 4 '   No Existe    '.
       when 10 message 1 3 '   No Existe    Lista de Precio.'.
       when 11 message 1 3 'Lista de Precio.   Sin Datos    '.
       when 12 message 1 3 '  Familia sin      Productos    '.
       when 13 message 1 3 '    Producto      sin Familia   '.
       when 14 message 1 3 '    Producto     sin SubFamilia '.
       when 15 message 1 3 '*** Cliente ****** Bloqueado ***'.

MUESTRA-ERROR-0100.
;------------------
    examine punt-error
       when 16 message 1 1 '    No Puede      Ingresar mas       Itemes     ****************' wait 1 second.
       when 17 message 1 1 '    Producto      No Existe en  Lista de Precios'.
       when 18 message 1 4 '* Rut Invalido *'.
       when 19 message 1 1 '   No Existen    Formas de Pago Para Seleccionar****************' wait 1 second.
       when 20 message 1 1 ' Cliente Nuevo   Solo Condicion     de Pago         EFECTIVO    ' wait 3 seconds.
       when 21 message 1 2 'Cliente no Tiene  Credito para      Comprar     ' wait 1 second.
       when 22 message 1 1 '**** ERROR *****  Al tratar de   Volver Familia     Original    ' wait 5 seconds.
       when 23 message 1 1 '**** ERROR *****  Al tratar de  Volver Sub-Famil    Original    ' wait 5 seconds.
       when 24 message 1 1 'No Puede Ingres.  Bonificacion  ****************Maximo Itemes OK' action exit.
       when 25 message 1 1 '**** ERROR *****   No Existe     Encabezado de       Pedido     '.
       when 26 message 1 2 '**** ERROR *****  Impresora No     esta Lista   '.
       when 27 message 1 3 '   No Existen       Pedidos     '.
       when 28 message 1 2 '   No Existen     Detalles de       Pedidos     '.
       when 29 message 1 2 '   No Existen      Datos  del       Producto    '.
       when 30 message 1 3 '   No Existen   Datos del Precio'.

MUESTRA-ERROR-0200.
;------------------
    examine punt-error
       when 31 message 1 3 '   Pedido Sin       Detalles    ' wait 1 second.
       when 32 message 1 1 '   MODALIDAD    ***************** Agrega  Item *****************' wait 1 second.
       when 33 message 1 4 ' Item ya Existe ' wait 1 second.
       when 34 message 1 1 '   MODALIDAD    ****************Revision  Pedido****************' wait 1 second.
;      when 27 message 1 1 '                '
;                          '                '
;                          '                '
;                          '                '.
    wait 1 second.
    move ER to last-key.
    exit.

MEMORIA-LLENA.
;-------------
    alarm.
    message 1 1 '   Advertencia  '.
    message 1 2 '****************'.
    message 1 3 ' Memoria  Llena '.
    message 1 4 ' Debe Descargar '.
    fast-action.
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                               COMUNICACIONES                               ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
COMUNICACIONES.
;--------------
    clear 3.
    message 1 4 'Espere .........'.
    move 'FILELIST'  to file-name.
    open update FILELIST-F
       when ER create FILELIST-F
               loop.
    reset FILELIST-F.
    move 'PC '      to tipo-host-1
    move 96         to velocidad-1
    move 0          to telefono
    move 'DEMOVTAS' to submit-file
    move 'DEMOVTAS' to identif-ptc
    move 0          to modo-comun
    move 1          to tipo-comun
    move 0          to tipo-modem
    move 'T'        to tipo-disc
    move 1          to i-borra-arch
    move 0          to estado-com
    write HOST-PARAMS-R.

LLAMA-RUTINA.
;------------
    move 'COMUNICA' to FILE-NAME.
    open ref PROGRAMA-F
       when ER move 1 to punt-error
               perform MUESTRA-ERROR
               exit.
    call PROGRAMA-F.
    move 'FILELIST' to FILE-NAME.
    erase FILELIST-F.
    perform LEE-INITIAL-SECTION.
    exit.

;ีออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออธ
;ณ                               ABRE ARCHIVOS                                ณ
;ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
ABRE-REF-PARAMETROS.
;-------------------
    move 'PARAMETR' to file-name.
    open REF PARAMETROS-F
       when ER move 3 to punt-error
               perform MUESTRA-ERROR.
    exit.

ABRE-UP-LOG.
;-----------
    move 'LOG' to file-name.
    open UPDATE LOG-F
       when ER create LOG-F
                  when MEM perform MEMORIA-LLENA
                           exit.
               loop.
    exit.

ABRE-REF-CIUDAD.
;---------------
    move 'CIUDAD' to file-name.
    open REF CIUDAD-F.
    exit.

ABRE-REF-CONDICION-DE-PAGO.
;--------------------------
    move 'CONDPAGO' to file-name.
    open REF CONDICION-PAGO-F.
    exit.

ABRE-REF-FAMILIA.
;----------------
    move 'FAMILIA' to file-name.
    open REF FAMILIA-F
       when ER move 4 to punt-error
               perform MUESTRA-ERROR.
    exit.

ABRE-REF-SUB-FAMILIA.
;--------------------
    move 'SUBFAMIL' to file-name.
    open REF SUB-FAMILIA-F
       when ER move 5 to punt-error
               perform MUESTRA-ERROR.
    exit.

ABRE-REF-CLIENTES.
;-----------------
    move 'CLIENTES' to file-name.
    open REF CLIENTES-F
       when ER move 6 to punt-error
               perform MUESTRA-ERROR.
    exit.

ABRE-REF-2CLIENTES.
;------------------
    move 'CLIENTES' to file-name.
    open REF CLIENTES-F.
    exit.

ABRE-REF-PRODUCTOS.
;------------------
    move 'PRODUC'      to file-1-producto.
    move w-bodega      to file-2-producto.
    move file-producto to file-name.
    open REF PRODUCTOS-F
       when ER move 7 to punt-error
               perform MUESTRA-ERROR.
    exit.

ABRE-REF-2PRODUCTOS.
;-------------------
    move 'PRODUC'        to file-1-producto.
    move w-revisa-bodega to file-2-producto.
    move file-producto   to file-name.
    open REF PRODUCTOS-F
       when ER move 7 to punt-error
               perform MUESTRA-ERROR.
    exit.

ABRE-REF-LIS-DISP.
;-----------------
    move 'LISTAS' to file-name.
    open REF LISTAS-DISPONIBLES-F
       when ER move 8 to punt-error
               perform MUESTRA-ERROR.
    exit.

ABRE-REF-BANCO.
;--------------
    move 'BANCOS' to file-name.
    open REF BANCO-F.
    exit.

ABRE-REF-PLAZA.
;--------------
    move 'PLAZAS' to file-name.
    open REF PLAZA-F.
    exit.

ABRE-REF-LISTA-PRECIO.
;---------------------
    move 'LIS'          to file-1-lis-pre.
    move w-lista-precio to file-2-lis-pre.
    move file-lis-pre   to file-name.
    open REF LISTA-PRECIO-F
       when ER move 10 to punt-error
               perform MUESTRA-ERROR.
    exit.

ABRE-REF-BONIFICACION.
;---------------------
    move 'BONIFICA' to file-name.
    open REF BONIFICACION-F.
    exit.

ABRE-UP-BONIFICACION.
;--------------------
    move 'BONIFICA' to file-name.
    open UPDATE BONIFICACION-F
       when ER create BONIFICACION-F
                  when MEM perform MEMORIA-LLENA
                           exit.
               loop.
    exit.

ABRE-UP-CABECERA.
;----------------
    move 'CABECERA' to file-name.
    open UPDATE CABECERA-PEDIDO-F
       when ER create CABECERA-PEDIDO-F
                  when MEM perform MEMORIA-LLENA
                           exit.
               loop.
    exit.

ABRE-REF-CABECERA.
;-----------------
    move 'CABECERA' to file-name.
    open REF CABECERA-PEDIDO-F.
    exit.

ABRE-UP-DETALLE-PEDIDO.
;----------------------
    move 'DETALLES' to file-name.
    open UPDATE DETALLE-PEDIDO-F
       when ER create DETALLE-PEDIDO-F
                  when MEM perform MEMORIA-LLENA
                           exit.
               loop.
    exit.

ABRE-REF-DETALLE-PEDIDO.
;-----------------------
    move 'DETALLES' to file-name.
    open REF DETALLE-PEDIDO-F.
    exit.

ABRE-REF-CLTE-NUEVO.
;-------------------
    move 'CLINUEVO' to file-name.
    open REF CLIENTE-NUEVO-F.
    exit.

ABRE-UP-CLTE-NUEVO.
;------------------
    move 'CLINUEVO' to file-name.
    open UPDATE CLIENTE-NUEVO-F
       when ER create CLIENTE-NUEVO-F
                  when MEM perform MEMORIA-LLENA
                           exit.
               loop.
    exit.

ABRE-UP-PAGOS-VENTA.
;-------------------
    move 'PAGOSVTA' to file-name.
    open UPDATE PAGO-VENTA-F
       when ER create PAGO-VENTA-F
                  when MEM perform MEMORIA-LLENA
                           exit.
               loop.
    exit.

ABRE-UP-VISITAS.
;---------------
    move 'VISITAS' to file-name.
    open UPDATE VISITAS-F
       when ER create VISITAS-F
                  when MEM perform MEMORIA-LLENA
                           exit.
               loop.
    exit.

ABRE-UP-OBSERVACIONES.
;---------------------
    move 'OBSERVA' to file-name.
    open UPDATE OBSERVACIONES-F
       when ER create OBSERVACIONES-F
                  when MEM perform MEMORIA-LLENA
                           exit.
               loop.
    exit.

ABRE-REF-OBSERVACIONES.
;----------------------
    move 'OBSERVA' to file-name.
    open REF OBSERVACIONES-F.
    exit.

END-OF-PROGRAM.
