;Speedy 1050 ROM-Listing
;(c)1986 Compy-Shop

	opt h- o+ c+ f+

;         *= $E000
	org $e000

VERSION   = $10


;Vom System verwendete Zero-Page Adressen:

MERK1     = $00
MERK2     = $01
MERK3     = $02
DLYT1     = $03                    ;Timer LO fuer Motor-Timer Routine
DLYT2     = $04                    ;Timer HI fuer Motor-Timer Routine
LDSW      = $05                    ;Letzte 'Dumm'-Schalter Position
WRKEN     = $06                    ;Anzahl der zu schreibenden Sectoren im Ram
EXSECT    = $07                    ;Sector # der Daten in Extended Buffer
DUMKEN    = $08                    ;Drive 'Dumm'-Status
FORKEN    = $09                    ;Aktuelles Density 0=DD,41=MD,82=SD
FORKEN2   = $0A                    ;Density fuer Format, wird von COM4F gesetzt
LWRTRA    = $0B                    ;Track # der zu schreibenden Sectoren im RAM
LTRACK    = $0C                    ;Track # des zuletzt gelesenen Sectors
TRACK     = $0D                    ;Aktuelle Track #
SECTOR    = $0E                    ;Aktuelle Sector #
CONST     = $0F                    ;Controller Status
DRSTAT    = $10                    ;Drive Status
COMST     = $11                    ;Command Status
RETRY     = $12                    ;Anzahl der Retry’s fuer Read/Write (normal 2)
RWLEN     = $13                    ;I/O Laenge
SECLEN    = $14                    ;Anzahl der Bytes pro Sector
USKEN     = $15                    ;Kennung fuer Uebertragungsgeschwindigkeit
DLYTIM    = $16                    ;Zeit, wie lange der Motor nach einem Befehl noch laeuft
STPTIM    = $17                    ;Verzoegerung fuer Steppermotor
COMPOS    = $18                    ;Position des letzten Befehls in Command-Tabelle
IND       = $19                    ;Indirekt-Vektor fuer Daten-Buffer
CHKSUM    = $1B                    ;Checksumme fuer Datenuebertragung
RDDATK    = $1C                    ;Kennung, ob Daten von Computer geholt werden muessen
KLAPPE    = $1D                    ;Letzte Klappen-Position
SECANZ    = $1F                    ;Sector-Anzahl pro Track,die vorhanden sein muessen
SECANZ1   = $1E                    ;Sector-Anzahl pro Track, die vorhanden sind
SECLST    = $20                    ;Sectorenliste
STALST    = $40                    ;Sectoren Statusliste
STPPOS    = $60                    ;Bit-Position fuer Steppermotor
DSPCTR    = $61                    ;Display/Drive-Controllbyte
BLOCKS    = $62                    ;Anzahl der Datenblocks fuer Datenuebertragung
IOIND     = $63

;Die Zero-Page Adressen $90-$CF sind unbenutzt

DATBUF    = $8C00                  ;Datenbuffer fuer Sectoren
EXBUF     = $9E00                  ;Extended-Buffer
CMTBL     = $9F00                  ;Command-Tabelle

RDPIO = $fd00
SDPIO = $fd27

;Fuer eigene Programme steht der Speicherbereich $8000-$8BFF zur Verfuegung

RESET     CLD                      ;* Kaltstart *
          LDX #$FF
          TXS                      ;Stackpointer neu setzen
          LDA #$38
          STA $0281                ;PADIR
          LDA #$38                 ;8=Motor aus
          STA $0280                ;PADAT
          LDA #$3D
          STA $0283                ;PBDIR
          STA $0282                ;PBDAT
          JSR CONRE2
          LDA #$80
          STA IND+1                ;Adresse $8000 setzen
          LDA #0
          STA IND
          LDX #$20
          TAY
DRAML     STA (IND),Y              ;Speicherbereich $8000-$9FFF loeschen
          INY
          BNE DRAML
          INC IND+1
          DEX
          BNE DRAML

          LDA #$60                 ;=RTS: Erweiterung der Reset-Routine vorgesehen
          STA CMTBL+$84
          LDA #$55
          STA $0401
          STA $0402
          LDX #$1E
RZS2      DEX
          BNE RZS2
          LDA $0401
          EOR $0402                ;Disk-Controller auf Funktionstuechtigkeit pruefen
          BNE SYSER0
          LDA #$48
          STA $0400
          LDX #$28
          JSR XWAIT
          LDA $0400
          LSR
          BCC SYSER0
          LDX #$28
          JSR XWAIT
          LDA $0400
          LSR
          BCC RESET2

SYSER0    JSR SYSERR               ;* System-Error Routine *
          BRA RESET2               ;* 2xBell und Reset     *

SYSERR    JSR BELL1                ;System-Error Routine                              
          LDX #$80                 ;gibt 2 x Bell aus
          JSR XWAIT
          JMP BELL1

RESET2    LDX #0                   ;* Reset-Einsprung *
DELL      STZ 0,X
          INX                      ;Zeropage  loeschen
          BNE DELL

          INC KLAPPE               ;=1 Initialisieren der System-Variablen
          INC LDSW                 ;=1
          INC BLOCKS               ;=1
          LDA #$40
          STA DLYTIM               ;Zeit fuer Motor-Timer Routine
          LDA #$2C
          STA STPTIM               ;Step—Zeit fuer Steppermotor testen
          LDA #$82                 ;Single Density - Status testen
          STA FORKEN
          STA FORKEN2
          JSR SDRDDP               ;System auf Single Density setzen und anzeigen
          JSR TROJUS               ;Kopf auf Track 0 positionieren
          JSR TRAANZ               ;und Track 0 anzeigen
          JSR CONRE2               ;2 x Controller Reset ausfuehren

          LDX #COMEND-COMTBL
COMMOV    LDA COMTBL,X             ;Kommando-Tabelle vom Rom
          STA CMTBL,X              ;ins Ram kopieren
          DEX
          BPL COMMOV

          LDA # <TSTCO2            ;Vektor fuer Ruecksprung in die Motor-Timer-Routine setzen
          STA CMTBL+$80            ;* Wenn ueber diesen Vektor in die Motor-Timer-Routine     *
          LDA # >TSTCO2            ;* gesprungen wird, muss zuvor in DLYT1 und DLYT2 die      *
          STA CMTBL+$81            ;* Zeit eingetragen werden,wie lange der Motor noch laufen *
          LDA # <MOTIM             ;* soll, wenn kein Kommando mehr kommt                     *
          STA CMTBL+$82
          LDA # >MOTIM
          STA CMTBL+$83
          JSR CMTBL+$84

BEREIT    CLD                      ;* Bereitschaftroutine                *
          LDX #$FF                 ;* Testet ob Diskette gewechselt wird *
          TXS
          JSR TSTDSW               ;'Dumm'-Schalter pruefen
          LDA WRKEN                ;noch Daten zu schreiben ?
          BNE TSTCO
          LDA $0400
          AND #$80                 ;Klappe pruefen
          CMP KLAPPE
          BEQ TSTCO
          STA KLAPPE               ;Klappen-Status setzen
          TAX
          BPL KLZU
          JSR MOTOFF               ;bei Klappe auf Motor ausschalten
          BRA CTSTAT
KLZU      LDA #$FF
          STA LWRTRA               ;Kennung fuer keine zu schreibenden Daten im RAM
          LDX #0
KZDL      STZ DATBUF+$80,X         ;2.Haelfte der Sector-Buffer Sector 1-3 fuer
          STZ DATBUF+$0180,X       ;Double Density loeschen
          STZ DATBUF+$0280,X
          INX
          BPL KZDL
          JSR TSTDEN               ;Density feststellen
          BMI TSTKLX
TKLOK     JSR RDSFOL               ;Sectorfolge lesen
TSTKLX    LDA #$FF
          STA DLYT2                ;Delay Timer High-Byte
CTSTAT    LDA $0400
          STA CONST                ;Controller-Status uebernehmen

TSTCO     JMP (CMTBL+$80)          ;($9F80) Vector fuer Kommando-Erkennungs-Routine
TSTCO2    LDA #2                   ;Kommando-Erkennungs-Routine
          BIT $0282
          BNE MOTIM                ;Computer aus
          BPL MOTIM                ;Command Bit

          LDA USKEN                ;Uebertragungsgeschwindigkeit feststellen
          AND #1
          ASL 
          STA IOIND                ;Kennung fuer Uebertragungsgeschwindigkeit setzen
          JMP RDINF                ;Befehl vom Computer empfangen

MOTIM     INC DLYT1                ;Motor-Timer-Routine
          BNE BEREIT
          INC DLYT2
          BEQ MOTTOF
          LDA DLYT2                ;bei Fast-Write nach ca. 2 Sekunden die Daten
          CMP #$98                 ;aus dem RAM-Speicher auf die Diskette schreiben
          BNE BEREIT
          LDA WRKEN
          BEQ BEREIT
MOTTOF   JSR TSTWR                ;noch Daten zu schreiben ?
          JSR MOTOFF               ;Motor ausschalten
          BRA BEREIT

MOTOFF   LDA #8                   ;Motor ausschalten
          TSB $0280                ;'Motor on' Bit zuruecksetzen
          LDA #$3C
          TSB $0282                ;Die 4 Bits des Steppermotors zuruecksetzen
          LDA #$10                 ;Drive-Status (Motor aus) setzen
          TRB DRSTAT
          RTS

TSTMON   BIT $0400                ;Klappe auf ?
          BMI TMOEX                ;ja
MOTON    PHA                      ;sonst Motor einschalten
          LDA #8                   ;'Motor on' Bit setzen
          TRB $0280
          BEQ TMOX
          LDA #$10                 ;Motor On Status setzen
          TSB DRSTAT
          LDX #5
          JSR X2WAIT               ;Verzoegerungsschleife, dem  Motor Zeit geben,
TMOX     PLA                      ;auf Touren zu kommen
          LDX #0                   ;OK-Status
TMOEX    RTS

TSTDEN   JSR TSTMON               ;* Density von Diskette feststellen *
          LDA #$2C
          STA STPTIM               ;Zeit fuer Stepwechsel kurz setzen
          JSR TRACK0               ;Kopf auf Track 0 positionieren
          STZ TRACK
TSTD0    LDY #0                   ;OK-Kennung
TSTD1    BIT $0400
          BMI TDERR
          PHY                      ;Status retten
          JSR TRADJA               ;Kopf positionieren
          PLY
          LDA #$20
          TRB $0280                ;Set MFM
TSTDER   JSR RDHDV                ;Sector lesen
          BCS SETFM                ;Lesefehler
          LDA #$41                 ;Medium Density - Status
          LDX $7D                  ;Sector Laenge
          BMI TSTDER               ;Daten ungueltig
          BEQ SMFMF
          LDA #0                   ;Double Density - Status
SMFMF    STA FORKEN
          BRA TSTDEX
SETFM    LDA #$20
          TSB $0280                ;Set FM
          LDA #$82
          STA FORKEN
RDHSD    JSR RDHDV                ;Sector Header lesen (SD)
          BCC TSTDEX               ;OK ?
          LDA TRACK
          CMP #3                   ;Density bis Track 3 suchen
          BEQ TDERR
          INC TRACK                ;Track # 1
          BRA TSTD1
TDERR    JSR RSFE80               ;Drive 'Dumm'-Status setzen
          LDY #$80                 ;Error-Kennung
TSTDEX   PHY
          JSR SDRDDP               ;Drive Density und Read/Write-Laenge einstellen
          PLA
          RTS 

SDRDDP   JSR DENDSP               ;Density auf Display anzeigen
SETDRD   LDA #$20                 ;DD
          TRB $0280                ;MFM
          LDX #18                  ;18 Sektoren/Track
          LDY FORKEN
          BEQ SDRD
          BMI SSD                  ;SD
          LDA #$80                 ;MD
          LDX #26                  ;26 Sektoren/Track
          BRA SDDL80
SSD      TSB $0280                ;Set FM
          LDA #0
SDDL80   LDY #$80                 ;128 Bytes/Sector
SDRD     STA DRSTAT               ;Drive-Status setzen
          STX SECANZ               ;Sectoren/Track setzen
          STY SECLEN               ;Bytes/Sector setzen
          RTS

;Sectorfolge auf aktuellem Track lesen

RDSFOL   LDX #2
          JSR X2WAIT               ;warten bis Klappe vollstaendig geschlossen ist
RDSFO1   JSR RDHEAD
          BCS RSFE80
          LDA SECANZ               ;Sektoren/Track
          INA
          STA MERK1
          STZ SECANZ1
          LDA #$CF
          STA $029F                ;Timer setzen
RDSFL    JSR RDHD1
          BCS RDSFT
          LDA $7A
          BMI RDSFL
          CMP TRACK                ;Track Nummer ueberpruefen
          BNE RSFE80
          LDA $7C                  ;SECTOR Nummer
          BEQ RDSFL                ;ungueltig
          BMI RDSFL                ;ungueltig
          CMP MERK1                ;>SECANZ ?
          BCS RSFE80
          LDX SECANZ1
          STA SECLST,X             ;Sector in Sectorliste eintragen
          BEQ RSFI
          LDX #0
RSFCL    CMP SECLST,X             ;Sector schon in Sectorliste ?
          BEQ RSFE80               ;ja - Dumm schalten
          INX 
          CPX SECANZ1
          BCC RSFCL
RSFI     INC SECANZ1
          BNE RDSFL
RDSFT    LDA SECANZ1              ;gefundene Sectoranzahl=vorgegebene Sectoranzahl ?
          CMP SECANZ  
          BNE RSFE80               ;nein - 'Dumm' schalten
RDSFOK   LDA DUMKEN               ;Status 'Sector-Folge OK'
          AND #$7F
SETDK    STA DUMKEN               ;Drive dennoch Dumm schalten ?
          AND #$38
          BNE DSPD2
SNEWTR   LDA #$FF
          STA LTRACK
          STZ EXSECT
          STZ WRKEN
          LDA #$2C                 ;Steppmotor Verzoegerungswert kurz stellen
          STA STPTIM
          CLC
          RTS
RSFE80    LDA #$80                 ;Status 'Sector-Folge ERROR'
          TSB DUMKEN
DSPDUM    JSR BELL1
DSPD2     LDA #$6D                 ;'SL' - Anzeige auf Display
          STA DISP10
          LDA #$38
          STA DISP1
          LDA #$64                 ;Steppmotor Verzoegerung normal
          STA STPTIM
          SEC     
          RTS

RDTRAV    JSR CALCTS               ;* Aktuellen Track ins ROM einiesen - mit Verify *
          BEQ RDTRVE               ;Sector # = 0
          BCS RDTRVE               ;Sector # groesser als zulaessig
          JSR RDTRA                ;alle Sectoren vom Track lesen
          BCS RDTRVE
RDTRV2    LDA SECANZ
          STA MERK1
RDTRVL    INX
          CPX SECANZ
          BCC RDTRT
          LDX #0
RDTRT     LDY SECLST,X             ;Sector-Status in Statusliste pruefen
          LDA STALST,Y
          BEQ RDTRD                ;Status OK
          JSR RDSS2                ;Sector noch einmal lesen
          BNE RDTRVE
RDTRD     DEC MERK1
          BNE RDTRVL
          CLC                      ;'Carry' = 0 OK-Status
          RTS
RDTRVE    SEC                      ;'Carry' = 1 Error-Status
          RTS

RDTRA     LDA #$40                 ;* Aktuellen Track ins RAM einlesen - ohne Verify *
          TSB DUMKEN
          JSR RDHDSP               ;Sector-Track-Position feststellen
          BCS RDTRAX
          LDX $7A
          CPX TRACK                ;Track # in Ordnung ?
          BNE RDTRAX
          LDX #0
RDTRSL    CMP SECLST,X             ;Sector in Liste suchen
          BEQ RDTRA2
          INX
          CPX SECANZ
          BCC RDTRSL
          BRA RDTRAX
RDTRA2    LDA SECANZ               ;Anzahl Sectoren/Track
          STA MERK1
          JSR SNEWTR               ;Kennung fuer neuen Track setzen
RDTR1L    JSR RDSSPE
          AND #$16                 ;CRC/AM-ERR zulassen
          BNE RDTRAX
RDTRA3    DEC MERK1
          BNE RDTR1L

          LDA #$40                 ;Track-Read Error-Status ruecksetzen
          TRB DUMKEN
          LDA TRACK
          STA LTRACK               ;Track # fuer zuletzt gelesenen Track merken
          CLC
          RTS
RDTRAX    JSR CONRES               ;Controller zuruecksetzen
          BIT $0400                ;Klappe auf ?
          SEC
          RTS

RDSSPE    INX                      ;* Unterprogramm zum lesen der Sectoren nach *
          CPX SECANZ               ;* Sector-Liste *
          BCC RDSS2
          LDX #0
RDSS2     LDA SECLST,X
          STA $0402                ;Sector #
          JSR SETBUF2              ;Buffer fuer Sector setzen
          JSR CONRES
          JSR RDSEC1               ;Sector lesen
          LDY SECLST,X             ;Sector-Status in Statusliste eintragen
          AND #$3F
          STA STALST,Y
          RTS

TROJUS    LDA #4                   ;* Track 0 Justierung *
          JSR TRADJ1               ;4 Tracks vorwaerts
TRACK0    JSR CONRE2
          LDY #$FF                 ;Step-Rueckwaerts-Kennung
SENSOR    LDA $0400
          AND #4                   ;Track-0 Sensor pruefen
          BEQ B5TST
          JSR TRVR                 ;1 Track zurueck
          BRA SENSOR
B5TST     LDA STPPOS               ;Track 0 nach Stepperposition fein justieren
          CMP #3
          BEQ SETTR0
          JSR TRVR
          BNE B5TST
SETTR0    STZ $0401                ;Track-Register des Controllers = 0
          LDX DLYTIM

XWAIT     LDA #18                  ;* Verzoegerungsschleife               *
XWA1      DEA                      ;* Wert der Verzoegerung im X-Register *
          BNE XWA1                 ;* 1 X-Wert ca. 100 Taktzyklen         *
          DEX
          BNE XWAIT
          RTS

X2WAIT    STX DLYT2                ;* Schleife fuer lange Verzoegerungen *
X2WA1     LDA #4                   ;* 1 X-Wert ca. 100000 Taktzyklen     *
          STA DLYT1
X2WA2     LDX #$FA
          JSR XWAIT
          DEC DLYT1
          BNE X2WA2
          DEC DLYT2
          BNE X2WA1
          RTS

TRVR      PHX                      ;* Trackwechsel Routine *
          LDX STPPOS
          INX                      ;* Y=00-7F  1 Step vorwaerts   *
          TYA                      ;* Y=80-FF  1 Step rueckwaerts *
          BMI RWARTS
          DEX
          DEX
RWARTS    TXA
          AND #3
          STA STPPOS               ;Bitposition des Steppermotors merken
          TAX
          LDA SMDAT,X              ;Bitmuster setzen
          STA $0282
          LDX STPTIM               ;Wert fuer Step-Verzoegerung
          JSR XWAIT
          PLX
RTN1      RTS
SMDAT    .BYTE $39,$35,$2D,$1D
TRADJA    JSR TRAANZ               ;Track-Justierung mit Trackanzeige
TRADJ     JSR CONRES               ;-- ohne Trackanzeige
          JSR TSTMON               ;Motor einschalten wenn Klappe geschlossen ist
          BMI JPE80                ;Klappe war auf
          LDA TRACK                ;neue Track #
TRADJ0    SEC
          SBC $0401                ;Track-Register Controller
          BEQ RTN1                 ;neuer Track=alter Track
          JSR TRAANZ               ;bei Trackwechsel - Trackanzeige
TRADJ1    TAY
          BPL TRADJ2
          EOR #$FF
          INA
TRADJ2    ASL                     ;Steps x2
          CMP #80
          BCC TRADJ3               ;mehr als 40 Tracks nicht zu lassen
          JSR SYSERR               ;2 x Bell ausgeben
          JSR TRACK0               ;Track 0 Justage
JPE80     JMP STELL2               ;=ERR80
TRADJ3    TAX
TRADJL    JSR TRVR                 ;1 Step ausfuehren
          DEX
          BNE TRADJL
SETTRN    LDX TRACK                ;Track # in Trackregister Controller kopieren
          STX $0401
          LDA #$10
          CPX #20
          BCC TRKL20               ;Track groesser 20
          TSB $0280                ;Controller umschalten
          BRA TRADJX
TRKL20    TRB $0280
TRADJX    LDX #$28                 ;Kurze Verzoegerung
          JMP XWAIT

CONRE2    JSR CONRES
CONRES    LDA #$D0                 ;Controller Reset
          STA $0400
          LDA #7
CONRL     DEA
          BNE CONRL

WREADY    LDA #1                   ;Warten auf Controller
WRDYL     AND $0400
          BNE WRDYL
          LDA $0400                ;Controller-Status
          RTS

RDINF     STZ COMST
          LDA #4                   ;4 Bytes nach Buffer $80
          LDX #$80
          LDY #0
          JSR RDBTS                ;Kommando vom Computer empfangen
CWAIT     LDA $0282                ;Warten bis Command - Leitung zurueckgesetzt ist
          BMI CWAIT
          LDX #4
          JSR XWAIT

          BIT COMST                ;Fehler bei Datenuebertragung ?
          BVS DELINF
          JSR TSTCOM               ;Kommando auf Gueltigkeit ueberpruefen
          BIT COMST
          BMI DELINF
          BVS ERR4E1
          JSR SEND41
          BIT RDDATK               ;Flag fuer Datenblock lesen
          BPL EXECCO
          JSR RDBYTS
          BIT COMST
          BVS ERR4E2
          JSR SEND41               ;'A' senden
EXECCO    JSR EXEC                 ;Kommando ausfuehren
          JSR CONRES
          LDA #3
          TRB DRSTAT               ;BIT 0+1=0
          LDA #$3C
          TSB $0282                ;Steppmotor aus

SDELAY    LDA DLYTIM               ;'Motor aus' Zeit setzen
          STA DLYT2
          STZ DLYT1


DELINF    STZ $80                  ;Kommandobuffer loeschen
          STZ $81
          STZ $82
          STZ $83
          JMP (CMTBL+$82)          ;Ruecksprungvector in Motor-Timer Routine

ERR4E1    LDA #1
          BRA ERR4E
ERR4E2    LDA #2
ERR4E     TSB DRSTAT
          JSR SEND4E               ;'N' senden
          BRA DELINF

DRKEN    .BYTE '3','2','4','1'        ;Laufwerksnummer Tabelle

ERR80     JMP STELL2

TSTCOM    LDA $0280                ;* Kommando vom Computer auf Gueltigkeit pruefen
          AND #3
          TAX
          LDA DRKEN,X              ;Drive # OK ?
          CMP $80
          BNE ERR80
          LDX #0
VERCOM    LDA CMTBL,X              ;Kommando in COM-Tabelle suchen
          BEQ ERR40                ;Ende COM-Tabelle
          ASL 
          ROR RDDATK
          LSR
          CMP $81                  ;Kommando gefunden ?
          BEQ COMFND
          INX
          INX
          INX
          BMI ERR40
          LDA DUMKEN               ;Kommando aus erweiterter Kommandotabelle zulassen ?
          AND #$38
          BEQ VERCOM
          CPX #$16
          BCC VERCOM
          BRA ERR40

COMFND    STX COMPOS
          CPX #9                   ;Kommando Position <3
          BCS TSTCOX
          JSR CALCTS               ;Track+Sector errechnen
          BCS ERR40                ;Sector unzulaessig
          BMI TSTCOX               ;RAM oder ROM-Adresse
          BNE COMF2
          LDA RDDATK
          BMI ERR40
          BRA TSTCOX
COMF2     LDA DUMKEN
          AND #$B8
          BNE WSEBUF
          JSR SETBUF               ;RAM-Buffer nach Sector # setzen
NOSEC     LDX COMPOS               ;Kommando = Read/Write Sector ?
          BEQ WRSTD
          CPX #3                   ;Write Sector verify ?
          BEQ WSEBUF
          LDA DUMKEN               ;Read Sector 'Dumm' geschaltet ?
          LSR
          BCC TSTCOX
          BCS WSEBUF
WRSTD     LDA DUMKEN               ;Write Sector ’Dumm' geschaltet ?
          AND #$FA
          BNE WSEBUF
          LDX SECTOR
          LDA TRACK                ;neuer Track=letzter Track ?
          CMP LWRTRA
          BEQ TSTRS                ;Sector # fuer Extended-Buffer merken
          STX EXSECT
WSEBUF    JSR SEXBUF               ;Extended Buffer als Sector-Buffer
          BRA TSTCOX
TSTRS     LDA WRKEN                ;schon ein Sector auf diesem Track geschrieben (ins RAM) ?
          BEQ TSTRS2
          LDA STALST,X             ;Sector schon einmal geschrieben ?
          BMI TSTCOX
TSTRS2    LDA #$80                 ;Write Status setzen
          STA STALST,X
          INC WRKEN                ;zu schreibende Sectoren+1
TSTCOX    LDA #0
          BIT RDDATK
          BPL RTN2
          INA
RTN2      TSB COMST
          RTS

ER40UK    INC USKEN                ;Umschalten zwischen normal oder High-Speed
ERR40     LDA $0296
          LDA #$40
          TSB COMST
          RTS

;Test, ob Klappe geschlossen und Write-Protect

TSTWRP    JSR CONRES
          AND #$C0
          RTS

TSTMEB    LDX EXSECT               ;Zu schreibender Sector im Extended-Buffer ?
          BEQ TMEBX
          LDA #$80
          STA STALST,X             ;Write-Status setzen
          JSR TRAANZ               ;Trackanzeige
          JSR SETBUF               ;RAM Buffer setzen
          LDY #0
MEBL      LDA EXBUF,Y              ;Sectordaten in Sectorbuffer kopieren
          STA (IND),Y
          INY
          CPY RWLEN
          BNE MEBL
          STZ EXSECT
          LDA #1
          STA WRKEN                ;Anzahl der zu schreibenden Sectoren =   1
          LDA TRACK                ;Track # fuer zu schreibenden Sector merken
          STA LWRTRA
TMEBX     RTS


RD128B    LDA #$80                 ;128 Bytes
          .BYTE $2C                ;=BIT ABS. (Dummy)
RD256B    LDA #0                   ;256 Bytes empfangen
          LDX # <EXBUF
          LDY # >EXBUF
RDBTS     STA RWLEN
          STX IND
          STY IND+1
RDBYTS    STZ CHKSUM               ;Checksumme loeschen
RD1BLK    LDA #$90                 ;maximale Zeit fuer Datenuebertragung fest legen
          STA $029F
          LDY #0
RDBL      JSR RDBYTE               ;1 Byte vom Computer empfangen
          STA (IND),Y
          CLC
          ADC CHKSUM
          ADC #0
          STA CHKSUM
          INY
          CPY RWLEN                ;letztes Byte ?
          BNE RDBL
          JSR ADDBUF               ;mehrere Datenblocks lesen (z. B. COM 60) ?
          BNE RD1BLK
          JSR RDBYTE
RDEXIT    LDY $0296
          EOR CHKSUM               ;Checksumme OK ?
          BNE ER40UK
          STZ COMST                ;COM-Status 'Datenuebertragung Ok'
          JMP SETRWL

RDBYTE    LDX IOIND                ;1 Byte lesen
          JMP (RDIND,X)
RDIND     .WORD NORDB,USRDB,RDPIO  ;Tabelle I/O - Routinen
;           (RDPIO fuer spaetere Erweiterung vorgesehen)





SEND41    LDA #$41                 ;'A' Status-Rueckmeldung an den Computer
          BNE SENDW
SEND43    LDA #$43                 ;'C'
          BNE SENDW
SEND45    LDA #$45                 ;'E'
          BNE SENDW
SEND4E    LDA #$4E                 ;'N'
SENDW     STA MERK1
          LDX #2
          JSR XWAIT
          BRA SDBYT2

SD128B    LDA #$80                 ;128 Bytes
          .BYTE $2C                ;=BIT ABS.
SD256B    LDA #0                   ;256 Bytes senden
          LDX # <EXBUF
          LDY # >EXBUF
SDBTS     STA RWLEN
          STX IND
          STY IND+1

;Datenblocks zum Computer senden

SDBYTS    STZ CHKSUM               ;Checksum loeschen
SD1BLK    LDY #0
SDBL      LDA (IND),Y
          STA MERK1
          CLC
          ADC CHKSUM
          ADC #0
          STA CHKSUM
          JSR SDBYT2
          INY
          CPY RWLEN
          BNE SDBL
          JSR ADDBUF
          BNE SD1BLK
          LDA CHKSUM               ;Cheksumme senden

SDBYTE    STA MERK1
SDBYT2    LDX IOIND                ;1 Byte senden
          JMP (SDIND,X)
SDIND     .WORD NOSDB,USSDB,SDPIO  ;Tabelle der I/O - Routinen
;          (SDPIO = fuer spaetere Erweiterung vorgesehen)



STELL     LDA #2                   ;2 Versuche setzen
          STA RETRY
STELL2    LDA #$80                 ;Command-Error setzen
          TSB COMST
          RTS

QUITT     LDA $0400                ;* Quittungsbyte an Computer senden *
QUITT2    STA CONST
          BIT COMST
          BMI SERR45
          LDA #$44
          TRB DRSTAT
          JMP SEND43               ;'C' Senden
SERR45    JSR ERRDSP
          JSR BELL
          LDA #4
          TSB DRSTAT
          JMP SEND45               ;'E' Senden

SETTIM    STA $0296                ;Set Timer Routine
          STA $029F
          RTS

TSTDSW    LDA $0280                ;'Dumm' Schalter abfragen
          AND #4
          CMP LDSW
          BEQ TSTDSX               ;gleiche Stellung wie vorher
          STA LDSW
          TAY
          BNE NODSW
          LDA #8                   ;Dumm - Modus setzen
          TSB DUMKEN
          JSR DSPD2                ;'SL' anzeigen
          JMP TSTDAT               ;Testen ob noch Sectoren zu schreiben sind
NODSW     LDA DUMKEN               ;Dumm - Modus zuruecksetzen
          AND #$F7
          STA DUMKEN
          AND #$B0
          BNE NOFAST
          JSR SNEWTR               ;Kennung fuer 'kein Sector im RAM' setzen
          JMP TRAANZ               ;Track # neu anzeigen
NOFAST    JMP DSPD2
TSTDSX    RTS

EXEC      LDX COMPOS               ;Kommando ausfuehren
          JMP (CMTBL+1,X)

;Normale Kommando - Tabelle:

COMTBL    .BYTE $D0                ;Write Sector
          .WORD COM50
          .BYTE $D7                ;Write Sector+Verify
          .WORD COM50
          .BYTE $52                ;Read Sector
          .WORD COM52
          .BYTE $53                ;Drive Status
          .WORD COM53
          .BYTE $21                ;Format Single/Double
          .WORD COM21
          .BYTE $22                ;Format Medium
          .WORD COM22
          .BYTE $4E                ;Read Drive - Options
          .WORD COM4E
          .BYTE $4F                ;Write Drive - Options
          .WORD COM4F

;Erweiterte Kommando-Tabelle:

          .BYTE $3F                ;Read High-Speed-Wert
          .WORD  COM3F
          .BYTE $44                ;Display/Bell/Drive Control
          .WORD COM44
          .BYTE $4C                ;Jump Adresse
          .WORD COM4C
          .BYTE $4D                ;Jump/Quitt
          .WORD COM4D
          .BYTE $51                ;Write all + Stop Motor
          .WORD COM51
          .BYTE $4B                ;Set/Reset 'Dumm'
          .WORD COM4B
          .BYTE $60                ;Write Track (normal Speed)
          .WORD COM60
          .BYTE $62                ;Read Track
          .WORD COM62
          .BYTE $68                ;SIO-Laenge senden
          .WORD COM68
          .BYTE $69                ;SIO Routine senden
          .WORD COM69
          .BYTE $41                ;Kommando einfuegen/loeschen
          .WORD COM41
          .BYTE $20                ;Spezial Format
          .WORD COM20
COMEND    .BYTE 0

;Bedeutung der Bits bei COM 44

;$80 : Error Anzeige zulassen
;$40 : Trackanzeige in Hexa-Dezimal
;$20 : Format ohne Verify
;$10 : bei COM 20 Sectoren 1,2,3,360,1024 nicht schreiben
;$08 : bei COM 51 Motor anlassen
;$01 : Bell bei Error zulassen

COM44     LDA $82                  ;* Display/Drive Kontrolle neu setzen *
          STA DSPCTR
          JMP SEND43

COM4B     JSR TSTDAT               ;* Slow/Fast - Mode Kontrolle *
          LDA $82
          STA DUMKEN
          AND #$B8
          BEQ C4BHI
          JSR DSPDUM               ;'SL' anzeigen
          BRA COM4BX
C4BHI     JSR COPSLT
COM4BX    JMP SEND43               ;'C' Senden

TSTDAT    JSR TSTWR                ;Noch zu schreibende Sectoren schreiben
          LDX SECANZ
          LDA #$40
TSTDSL    STA STALST,X             ;Status 'kein Sector in RAM' setzen
          DEX
          BNE TSTDSL
          RTS

COM4D     JSR QUITT
COM4C     JMP ($82)                ;Sprung ueber 'Jump Adresse'

COM51     JSR TSTWR                ;noch zu schreibende Sectoren schreiben
          LDA DSPCTR
          AND #8                   ;Motor ausschalten ?
          BNE C51Q
          JSR MOTOFF
C51Q      JMP SEND43               ;'C' senden

COM62     LDA $83                  ;RAM - oder ROM - Adresse ?
          BMI C62X
          JSR STELL2               ;Error - Status setzen
          JSR RDTRAV               ;Read Track mit Verify
          BCS C62X
C620K     STZ COMST                ;OK Status senden
C62X      JSR QUITT                ;Quittung senden
          LDA SECANZ
          STA BLOCKS               ;Anzahl der Datenblocks setzen
          LDA SECLEN
          LDX $82
          LDY $83                  ;RAM - oder ROM - Adresse ?
          BMI C62SD
          LDX # <DATBUF
          LDY # >DATBUF
C62SD     JMP SDBTS                ;alle Datenblocks senden

COM60     JSR STELL2               ;Error Status setzen
          JSR CALCTS
          BEQ C60X                 ;Sector 0 nicht zulassen
          BCS C60X                 ;Track >39
          BMI C60RD                ;Daten ins RAM
          LDA SECTOR
          DEA
          BNE C60X
          LDA # <DATBUF
          STA IND
          LDA # >DATBUF
          STA IND+1
C60RD     LDA SECANZ
          LDX SECLEN
          BEQ C60RD2
          LSR
C60RD2    STA BLOCKS               ;Anzahl der Datenblocks setzen
          STZ RWLEN
          JSR RDBYTS               ;alle Datenblocks lesen
          BIT COMST                ;Fehler in Datenuebertragung ?
          BVS C60E4E
          JSR SEND41               ;'A' Senden
          LDA $83
          BMI C60OK
          LDA TRACK                ;Track # uebernehmen
          STA LWRTRA
          JSR TSTWRP               ;Write Protect oder Klappe auf ?
          BNE C60X
          LDX SECANZ
          STX WRKEN
          LDA #$80                 ;Write Status fuer alle Sectoren setzen
C60L      STA STALST, X
          DEX
          BNE C60L
          JSR TSTWR                ;den ganzen Track schreiben
          LDA MERK2
          BNE C60X
C60OK     STZ COMST                ;OK Status setzen
C60X      JMP QUITT                ;Quittung senden
C60E4E    JMP SEND4E               ;'N' Senden

COM68     JSR SEND43               ;* Diese Routine gibt die Laenge der SIO-Rouitne *
          LDA #2                   ;* an den Computer zurueck                       *
          LDX # <SIOLEN
          LDY # >SIOLEN
          JMP SDBTS
SIOLEN    .WORD SIOEND-SIO

COM69     LDA # <SIO               ;* Routine zum Senden der kompletten SIO-Routine an den *
          STA IND                  ;* Computer                                             *
          SEC
          SBC $82
          STA $82
          LDA # >SIO               ;Unterschied zwischen ORG- und TARGET-Adresse errechnen
          STA IND+1
          SBC $83
          STA $83
          JSR SEND43
          LDY #0
          STY CHKSUM
C69L      LDA ABSTBL,Y             ;eine zu relocierende Adresse ?
          CMP IND
          BNE C69SB
          LDA ABSTBL+1,Y
          CMP IND+1
          BNE C69SB
          JSR C69LDB
          SEC                      ;absolute Adresse relocieren
          SBC $82
          PHP
          JSR C69SDB
          JSR C69LDB
          PLP
          SBC $83
          JSR C69SDB
          INY
          INY
          BNE C69TE
C69SB     JSR C69LDB
          JSR C69SDB
C69TE     LDA IND
          CMP # <SIOEND            ;Ende der SIO-Routine
          BNE C69L
          LDA IND+1
          CMP # >SIOEND
          BNE C69L
          LDA CHKSUM
          JMP SDBYTE
C69LDB    LDA (IND)                ;naechstes Byte der SIO-Routine empfangen
          INC IND
          BNE C69LDX
          INC IND+1
C69LDX    RTS
C69SDB    STA MERK1                ;1 Byte zum Computer senden
          CLC
          ADC CHKSUM
          ADC #0
          STA CHKSUM
          JMP SDBYT2

COM41     LDA #3                   ;3 Bytes in den Extended-Buffer holen
          LDX # <EXBUF
          LDY # >EXBUF
          JSR RDBTS
          BIT COMST                ;Fehler bei Datenuebertragung ?
          BVS C41E4E
          JSR SEND41               ;'A' senden
          JSR STELL2
          LDA EXBUF
          AND #$7F
          STA MERK1                ;Kommando ohne Bit 7 merken
          LDX #0
C41SL     LDA CMTBL,X              ;Testen ob das Kommando schon in der Kommando-Tabelle ist
          PHP
          BEQ C41AC2               ;Tabellenende
          AND #$7F
          CMP MERK1
          BEQ C41AC1
          PLP
          INX
          INX
          INX
          CPX #126
          BCC C41SL
          BRA C41X                 ;Command-Tabelle voll
C41AC1    LDA EXBUF+1              ;Command-Adresse = 0000 ?
          ORA EXBUF+2
          BNE C41AC2
C41ML     LDA CMTBL+3,X            ;Kommando loeschen und Tabelle kuerzen
          STA CMTBL,X
          INX
          CPX #123
          BCC C41ML
          PLP
          BRA C41OK
C41AC2    LDA EXBUF                ;Kommando anhaengen
          STA CMTBL,X
          LDA EXBUF+1
          STA CMTBL+1,X
          LDA EXBUF+2
          STA CMTBL+2,X
          PLP                      ;war eine '0' am Tabellenende vorhanden ?
          BNE C41OK
          STZ CMTBL+3,X            ;wieder ein '0' anhaengen
C41OK     STZ COMST
C41X      JMP QUITT                ;Quittung senden
C41E4E    JMP SEND4E

;Spezial Formatierungsroutine
;'Complete' wird sofort zurueckgegeben
;Das Laufwerk schreibt bei Bedarf selbststaendig
;die Sectoren  1,2,3,360 und 1024 (mit COM44, Bit 4 einzustellen)

COM20     JSR STELL2
          JSR TSTWRP               ;Klappe + Write-Protect testen
          BNE C20E1
          STZ COMST
C20E1     JSR QUITT                ;Quittung vor dem Formatieren zuruecksenden
          LDA COMST
          BNE RTN4
          BIT FORKEN2
          BVS GC22
          JSR COM21                ;Format SD oder DD
          BRA TFORER
C20ERR    JMP BELL1
RTN4      RTS
GC22      JSR COM22                ;Format MD
TFORER    LDA COMST
          BNE C20ERR
          LDA DSPCTR
          AND #$10                 ;Display/Drive Control - Bit 4 gesetzt ?
          BNE WRVTX
          LDX #0                   ;ja, Sectoren nicht schreiben
C20ML     LDA BOOTID,X             ;Daten fuer Sector 1 in den Extended-Buffer kopieren
          STA EXBUF,X
          STZ EXBUF+$80,X          ;fuer DD 2.Sectorhaelfte loeschen
          INX
          BPL C20ML
          JSR SEXBUF               ;Extended Buffer setzen
          LDA #1
          JSR WRSECN               ;Sector 1 schreiben
          LDX #0
C20M2L    LDA BOOTID+$80,X         ;Daten fuer Sector 2 in den Extended-Buffer kopieren
          STA EXBUF,X
          INX
          BPL C20M2L
          LDA #2
          JSR WRSECN               ;Sector 2 schreiben
          LDX #0
C20M3L    LDA BOOTID+$0100,X       ;Daten fuer Sector 3 in den Extended-Buffer kopieren
          STA EXBUF,X
          INX
          CPX # <(BIDEND-BOOTID-$80)   ;Bufferende loeschen
          BNE C20M3L
C20DL     STZ EXBUF,X
          INX
          BNE C20DL
          LDA #3
          JSR WRSECN               ;Sector 3 schreiben
          LDX #11
C20SVT    LDA VTTBL1,X
          LDY VTTBL2,X
C20VTL    STA (IND)                ;VTOC erzeugen
          INC IND
          DEY
          BNE C20VTL
          DEX
          BPL C20SVT
          LDA #$68
          STA $82
          LDA #1
          STA $83
          JSR CALCTS               ;Track errechnen
          JSR TRADJA               ;Kopf positionieren + Track # anzeigen
          BIT FORKEN2
          BVC WRVTOC
          LDA #$F2
          STA EXBUF+1
          LDA #3
          STA EXBUF+2
WRVTOC    JSR SEXBUF
          JSR WRSECT               ;Sector 360 (VTOC) schreiben
          BIT FORKEN2              ;Medium Density ?
          BVS C20MD                ;ja.
WRVTX     RTS
C20MD     LDA #39
          STA TRACK
          JSR TRADJA               ;Track 39 positionieren
          LDX #8
MDVTL     LDY MDVT2,X
          LDA MDVT1,X
MDVTL2    STA (IND)                ;VTOC 2 (fuer Dos 2.5 Format) erzeugen
          INC IND
          DEY
          BNE MDVTL2
          DEX
          BPL MDVTL
          JSR SEXBUF
          LDA #10
          JMP WRSECN               ;Sector 1024 schreiben (nur bei MD)

;Daten fuer VTOC - Single Density

VTTBL1   .BYTE 0,$FF,$7F,0,$FF,15,0,2,$C3,2,$C3,2



VTTBL2   .BYTE $9C,43,1,1,44,1,5,1,1,1,1,1



;Daten fuer VTOC - Medium Density

MDVT1    .BYTE 0,1,$2F,$FF,$7F,$FF,$7F,0,$FF



MDVT2    .BYTE 4,1,1,37,1,43,1,1,39



;Es folgt das Bootprogramm fuer die Sectoren 1-3

COLOR         = $D6
MASK      = $E1

BOOTID   .BYTE 0,3,0,7,$77,$E4


          LDA # <(IDTEXT-BOOTID+$0700)
          STA $00
          LDA # >(IDTEXT-BOOTID+$0700)
          STA $01
          LDA #$50                 ;Screen-Buffer $5000
          STA $D5
          LDX #0
          STX $D4
          LDY #0
L2        LDA ($00,X)              ;Text in Screen-Buffer kopieren
          BMI OV                   ;naechste Zeile
          STA ($D4),Y
LLP       INY
          INC $00
          BNE L2
          INC $01
          BNE L2

OV        CMP #$FF                 ;Text-Ende ?
          BEQ PICOK
          AND #$7F                 ;X-Position naechste Zeile uebernehmen
          TAY
          LDA $D4
          ADC #40                  ;Screen-Buffer fuer 1 Zeile heraufzaehlen
          STA $D4
          BCC LLP
          INC $D5
          BNE LLP

PICOK     LDA # <(DLIST-BOOTID+$0700)     ;Display-List fuer Screen-Buffer setzen
          STA $0230
          LDA # >(DLIST-BOOTID+$0700)
          STA $0231
          LDA #0
          STA $02C8
          STA $02C6
          STA $D40E                ;Antic-Zugriff ausschalten
          STA COLOR
          STA MASK
          LDA #$20
          STA $02F4                ;Zeichensatz-Basisadresse auf $2000
          LDA # <(DLI-BOOTID+$0700)
          STA $0200
          LDA # >(DLI-BOOTID+$0700)
          STA $0201
DS        LDA #$C0
          STA $D40E                ;Antic-Zugriff und DLI zulassen
LP        LDY $D20A
          LDA $E000,Y              ;Zeichensatz per Bit-Mapping kopieren
          JSR CODE-BOOTID+$0700
          STA $2000,Y
          LDY $D20A
          LDA $E100,Y
          JSR CODE-BOOTID+$0700
          STA $2100,Y
          LDY $D20A
          LDA $E300,Y
          JSR CODE-BOOTID+$0700
          STA $2300,Y
          JMP LP-BOOTID+$0700
CODE      PHA
          INC COLOR
          LDA COLOR
          BIT DS-BOOTID+$0701
          BPL A1
          BVC A1
          LDA #0
          STA COLOR
          INC MASK                 ;Bit-Maske + 1
          BNE A1
          DEC MASK
A1        LDA MASK
          CMP #$FF                 ;alle Bits gesetzt
          BNE DO
          PLA
          RTS
DO        LDA $D20A
          AND MASK
          STA $E0
          PLA
          AND $E0                  ;Bits vom Zeichensatz ausblenden
          RTS
DLI       PHA
          TYA
          PHA
          LDY #15
DLIL      TYA
          ORA #$20
          STA $D40A
          STA $D016                ;setzen der Farbhelligkeit
          EOR #$AF
          STA $D017                ;Setzen der Helligkeit fuer Schrift
          DEY
          BPL DLIL
          LDA #$0C
          STA $D017
          PLA
          TAY
          PLA
          RTI

;Texte der Bootsectoren

IDTEXT   .BYTE "Disk formatiert mit:"




          .BYTE $81
          .BYTE "SPEEDY "

          .BYTE 81,80,85,80,0

          .BYTE "V"
          .BYTE [VERSION/16]^16
          .BYTE "."
          .BYTE VERSION&15^16
          .BYTE $83
          .BYTE "(c) Bibosoft -- COMPY SHOP 1986"







          .BYTE $80
          .BYTE "Weitere Informationen bei:"






          .BYTE $90
          .BYTE "COMPY SHOP"


          .BYTE $90
          .BYTE "Tel.: 0208/497169"




          .BYTE $FF

;Display - List

DLIST    .BYTE $70,$70,$70,$70,$70,$70,$70,$42,0,$50


          .BYTE $70,$70,$F0,7,6,$70,$70,2,$70,$70,2,$70,$70,2,2,$41



           .WORD DLIST-BOOTID+$0700


BIDEND


SEXBUF    LDA # <EXBUF             ;IND-Vector auf den Extended-Buffer setzen
          STA IND
          LDA # >EXBUF
          STA IND+1
          RTS

CALCTS    LDA $82                  ;Routine zum errechnen der Track- und Sectornummer
          STA IND
          LDA $83
          STA IND+1
          BMI CTSD                 ;Sector # >$7FFF -> Buffer Adresse ROM/RAM
          ORA $82                  ;Sector 0 ?
          BNE CALC2                ;nein
CTSD      LDA DUMKEN
          AND #$38
          CMP #1                   ;Bei 'Dumm' Carry setzen
          LDA $82
          ORA $83
          RTS
CALC2     LDY #$FF
CALCL     INY
          LDA IND
          STA SECTOR
          SEC
          SBC SECANZ
          STA IND
          BCS ADDTRA
          DEC IND+1
          BMI CALCT
ADDTRA    ORA IND+1
          BNE CALCL
CALCT     CPY #40                  ;'Carry' - Flag fuer Sector # - Error setzen
          STY TRACK
          LDA SECTOR
CALCX     RTS

SETBUF    LDA SECTOR                ;* Diese Routine errechnet den RAM-Buffer *
SETBUF2   STZ IND                   ;* aus der Sectornummer                   *
          DEA
          CLC
          BIT SECLEN
          BPL ADDHIB
          LSR
          ROR IND
ADDHIB    ADC # >DATBUF
          STA IND+1
SETBX     RTS

SETRWL    LDY SECLEN               ;* Routine zum richtigen setzen der Read/Write - Laenge *
          LDA $83
          BNE SRWL
          LDA $82
          CMP #4                   ;Sector # < 4 ?
          BCS SRWL
          LDY #$80                 ;128 Bytes
SRWL      STY RWLEN
          LDA #1
          STA BLOCKS               ;Anzahl der Datenblocks = 1
          RTS

ADDBUF    LDA BLOCKS               ;* Buffer erhoehen falls meherere Daten-Buffer uebertragen *
          DEA                      ;* werden sollen                                           *
          BEQ ADDB4
          LDA RWLEN
          BEQ ADDB2
          CLC
          ADC IND
          STA IND
          BCC ADDB3
ADDB2     INC IND+1
ADDB3     DEC BLOCKS
ADDB4     RTS

COM52     LDA $83                  ;* READ-SECTOR Routine *
          BMI C52OK                ;verzweigen bei Read RAM oder ROM
          ORA $82
          BEQ C52OK                ;verzweigen bei Read Zero-Page

          JSR STELL                ;Retry's und ERROR-Status setzen
          JSR CONRES
          BMI C52X
          LDA DUMKEN               ;Wenn 'Dumm'-geschaltet, den Sector normal lesen
          AND #$B9
          BNE C52TRA

          LDA TRACK                ;richtiger Track schon im RAM ?
          CMP LTRACK
          BEQ TRAOK
C52TRR    JSR TSTWR                ;muss ein neuer Track komplett eingelesen werden, RAM vorher
          JSR RDTRA                ;auf noch zu schreibende Sectoren pruefen
TRAOK     BIT DUMKEN
          BVS C52DUM
          LDX SECTOR
          LDA STALST,X             ;Status des zu lesenden Sectors OK  ?
          CMP #$40
          BCS C52TRR               ;Sector befindet sich noch nicht im RAM
          AND #$1F
          BNE C52DUM
          JSR SETBUF               ;Buffer fuer entsprechenden Sector setzen
          LDA STALST,X
          BEQ C52OK
          JSR QUITT2               ;Quittung und Daten senden
          JMP SDBYTS

C52RTY    DEC RETRY                ;Retry bei Read-Sector
          BMI C52X
          AND #$2F
          BNE FLIP
          JSR TRACK0               ;bei defektem Datenfeld Track 0
          BRA C52TRA
FLIP      LDY #$FF                 ;1 Track Rueckwaerts
          JSR TRVR
          INY                      ;1 Track vorwaerts
          JSR TRVR
          BRA C52TRA

C52DUM    JSR SETBUF
C52TRA    JSR TRADJ                ;* Normale Read-Sector Routine *
          BNE C52X
          LDX SECTOR
          JSR RDSECT
          TAY
          LDA DUMKEN               ;wenn nicht 'Dumm' geschaltet
          AND #$B9                 ;Status in Status liste uebernehmen
          BNE C52NSS
          TYA
          STA STALST,X
C52NSS    TYA
          BNE C52RTY

C52OK     STZ COMST
C52X      JSR QUITT                ;Quittung und Daten senden
          JMP SDBYTS

RDSECT    LDA SECTOR               ;* Einzelnen Sector in vor bezeichneten RAM einlesen *
          STA $0402
RDSEC1    LDA #$88                 ;Read Sector fuer Controller
          STA $0400
          LDY #0
RDSST     LDA #$E6
          STA $029F                ;Timer setzen
RDSL      BIT $0280
          BVC RDSTO                ;Time out
          BPL RDSL
          LDA $0403                ;1 Byte vom Controller uebernehmen
          EOR #$FF
          STA (IND),Y              ;und in den Sectorbuffer schreiben
          LDA $0296
          INY
RDSL2     BIT $0280
          BVC RDSTO
          BPL RDSL2
          LDA $0403
          EOR #$FF
          STA (IND),Y
          INY
          CPY SECLEN
          BNE RDSL2
          JSR WREADY               ;Warten auf Controller 'Ready'
RDSRS     LDA $0400
          AND #$3E                 ;Status pruefen
          RTS
RDSTO     LDA $0400
          LSR                     ;'In Use' Flag noch gesetzt ?
          BCS RDSST
          LDY $0296
          LSR                     ;'Lost Data' Flag gesetzt ?
          LSR 
          BCS RDSEC1
          BRA RDSRS
RDHDSP    JSR TRADJ                ;* Diese Routine fuehrt 3 Header-Read Operationen aus  *
          BNE RTN3                 ;* und gibt bei erfolgreichem Leseversuch die Sector # *
RDHDS1    LDY #3                   ;* im Accu zurueck                                     *
RDHSL     JSR RDHDV
          LDA $7C
          BCC RTN3                 ;Leseversuch erfolgreich: 'Carry' = 0
          DEY
          BNE RDHSL
          SEC
RTN3      RTS

RDHDV     LDA #$20                 ;Zeit fuer den Controller setzen einen Header zu finden
          STA $029F
RDHDVL    JSR RDHD1
          BCS RDHVX                ;wird kein Header gefunden, so ist das 'Carry'-Flag gesetzt
          BNE RDHDVL
RDHVX     RTS

RDHEAD    LDA #$D8                 ;* Hier liegt die gleiche Funktion wie bei RDHDV vor, nur wird *
          STA $029F                ;* dem Controller etwas ueber eine komplette Umdreheung der    *
RDHD1     LDX #$7A                 ;* Diskette Zeit gegeben.                                      *
          LDA #$C8
          STA $0400                ;Der Header besteht aus 6 Bytes, die ab Adresse $7A abgelegt
RDHDL     BIT $0280                ;werden:
          BVC INFTO
          BPL RDHDL                ;1.Byte ($7A) Track Nr.
          LDA $0403                ;2.Byte ($7B) Side Nr.
          STA $00,X                ;3.Byte ($7C) Sector Nr.
          INX                      ;4.Byte ($7D) Sector laenge
          BPL RDHDL                ;5.+6. Byte ($7E,$7F) 2 CRC Bytes
          JSR WREADY
          AND #$0C
          CLC
          RTS
INFTO     JSR CONRES
	SEC
	RTS


;Die Sectoren, die als zu schreibend in der Statusliste gekennzeichnet sind
;auf die Diskette schreiben

TSTWR     LDA WRKEN                ;Anzahl der zu schreibenden Sectoren
          BEQ TWREX
          JSR MOTON                ;Motor zwingend einschalten
          LDA TRACK                ;aktuellen Track merken
          PHA
          LDA LWRTRA
          STA TRACK
          JSR TRAANZ               ;Track Nr. anzeigen.
          JSR TRADJ0               ;Kopf positionieren
          JSR RDHDS1               ;1. Header suchen
          PLX
          STX TRACK                ;Track # zurueckholen
          STZ MERK2
          BCS TWRERR
          LDX #0
TW1L      CMP SECLST,X             ;Sector in Sectorliste suchen
          BEQ NOWRS
          INX
          CPX SECANZ
          BCC TW1L
TWERR2    JSR BELL1                ;Sector nicht in Sectorliste gefunden, 1 Bell ausgeben
          BRA NOWRS

TSTWL     LDY SECLST,X
          LDA STALST,Y             ;ein zu schreibender Sector ?
          BPL NOWRS                ;nein
          JSR TSTWRP               ;Write Protect + Klappe testen
          BEQ TSTW1
          LDA MERK2
          BNE TSTW2
TWRERR    JSR WRERR                ;5 Sekunden auf dem Display herunterzaehlen und Bell ausgeben
          STA MERK2
          BRA NOWRS
TSTW1     TYA
          STA $0402                ;Sector # ins Sectorregister des Contollers schreiben
          JSR SETBUF2
          JSR WRSEC1               ;Sector schreiben
          BEQ TSTW2                ;Status OK
          JSR TSTWRP               ;Write Protect + Klappe testen
          BNE NOWRS
TSTW2     LDY SECLST,X             ;'Sector written'-Status setzen
          LDA #$40
          STA STALST,Y
          DEC WRKEN

NOWRS     INX                      ;naechste Position in Sectorliste setzen
          CPX SECANZ
          BCC TSTWE
          LDX #0
TSTWE     LDA WRKEN
          BNE TSTWL
          LDA #$FF
          STA LWRTRA               ;'Track written'-Status setzen
TWREX     RTS

;Diese Routine wird von der Write-Track Routine aufgerufen
;wenn die Diskette vor einer abgeschlossenen Write-Sequenz
;aus dem Laufwerk genommen oder Write-Protected wird.
;5 Sekunden werden auf dem Display herabgezaehlt und
;nach jeder Sekunde wird ein Warnton abgegeben

WRERR     PHX
          PHY
          LDY #5                   ;5 Sekunden setzen
WRERRL    TYA
          JSR ANZEIGE              ;Sekunden zur Anzeige bringen
          JSR BELL1                ;1 Bell ausgeben
          LDX #3                   ;grosse Verzoegerungsschleife (ca. 1 Sekunde) setzen
WEWL1     LDA #$FF
          STA $029F
WEWL2     JSR TSTWRP               ;noch Write-Protected oder Klappe auf ?
          BEQ WRERX
          BIT $0280
          BVS WEWL2
          DEX
          BNE WEWL1
          DEY
          BPL WRERRL
WRERX     JSR TRAANZ               ;aktuelle Track # wieder anzeigen
          LDX $0296                ;Timer IRQ zuruecksetzen
          PLY                      ;Y-Register zurueckholen
          PLX                      ;X-Register zurueckholen
          JMP TSTWRP

COM50     JSR STELL                ;* Write Sector Routine *
          LDA $83
          BMI C50OK2               ;Sector # > 7FFF -> RAM - oder ROM - Adresse
          LDA $81
          CMP #$57                 ;Write mit Verify ?
          BEQ C50DUM
          JSR TSTWRP               ;Write Protect + Klappe testen
          BNE C50X
          JSR TSTMON               ;Motor einschalten
          LDA DUMKEN
          AND #$FA                 ;'Dumm' geschaltet ?
          BNE C50DUM

          LDA TRACK                ;einen kompletten Track im RAM ?
          CMP LWRTRA
          BNE C50B
          LDA WRKEN
          CMP SECANZ
          BNE C50OK2
C50B      JSR TSTWR                ;alle zu schreibenden Sectoren schreiben
C50C      JSR TSTMEB               ;falls vorhanden, Sectordaten aus Extended Buffer
                                   ;in den Sectorbuffer kopieren
          BRA C50OK2

;Normale Write-Sector Routine

C50DUM    JSR TSTWRP               ;Disk 'Write Protect' oder Klappe auf ?
          BNE C50X                 ;js
          LDA TRACK
          CMP LTRACK
          BNE C50TRA
          LDX SECTOR               ;zu schreibenden Sector als im RAM stehende Daten Markieren
          STZ STALST,X             ;braucht bei 'Read Sector' nicht mehr gelesen zu werden
C50TRA    JSR TRADJ                ;Kopf positionieren
          BNE C50X
          JSR WRSECT               ;Sector vom angegebenen Buffer schreiben
          BEQ C50OK
          DEC RETRY
          BEQ C50X
          JSR TRACK0               ;Track 0 positionieren
          BRA C50DUM               ;Retry ausfuehren

C50OK     LDA $81
          CMP #$57                 ;Write mit Verify
          BNE C50OK2
          LDA DUMKEN
          AND #4                   ;Verify bei COM57 ausfuehren ?
          BNE C50OK2
          JSR VERSEC               ;Verify Sector
          BCS C50X
          BNE C50X
C50OK2    STZ COMST                ;OK-Status setzen
C50X      JMP QUITT                ;Quittung senden

WRSECT    LDA SECTOR               ;* Einzelnen Sector aus vorbezeichneter RAM-Adresse *
WRSECN    STA $0402                ;* auf die Diskette schreiben                       *
WRSEC1    LDY #0
          LDA #$A8                 ;Write Sector Befehl an Controller
          STA $0400
WRSST     LDA #$E6
          STA $029F                ;Timer setzen
WRSL      LDA (IND),Y              ;1 Byte invertiert
          EOR #$FF
WRSDR     BIT $0280
          BVC WRSTO                ;Time out
          BPL WRSDR
          STA $0403                ;Byte an den Controller uebergeben
          LDA $0296
          INY
          CPY SECLEN               ;alle Bytes geschrieben ?
          BNE WRSL
          JSR WREADY               ;auf Controller 'Ready' warten
          AND #$0C
          RTS
WRSTO     LDA $0400
          LSR                     ;'In Use' Bit gesetzt ?
          BCS WRSST
          LSR                     ;'Data Request' gesetzt ?
          LSR 
          BCS WRSEC1
          LDA $0400
          RTS

VERSEC    LDA SECTOR               ;* Sector mit Datenbuffer vergleichen *
          STA $0402
VERSE1    LDA #$88                 ;Read Sector an Controller
          STA $0400
          LDY #0
VERSST    LDA #$D8
          STA $029F                ;Timer setzen
VERSL     LDA (IND),Y
          EOR #$FF
VERSL2    BIT $0280
          BVC VERSTO               ;Time out
          BPL VERSL2
          CMP $0403
          BNE VERSER
          INY
          CPY SECLEN               ;alle Daten verglichen ?
          BNE VERSL
          JSR WREADY               ;auf Controller 'Ready' warten
          LDA #0                   ;alle Daten verglichen und OK
          CLC                      ;kein Fehler aufgetreten
          RTS
VERSER    JSR CONRES
          LDA #$80                 ;Daten ungleich
          CLC                      ;kein Lesefehler aufgetreten
          RTS
VERSTO    LDA $0400
          LSR                     ;Kommando noch 'In Use'
          BCS VERSST
          JSR CONRES
          SEC                      ;'Carry' = ERROR-Flag
          RTS

;Formatierungs-Routinen

COM22     LDA #$41                 ;Medium Density (MD)-Kennung
          BNE FSDATB

COM21     LDA $82
          CMP #$11                 ;Sector Nr. 1041 ?
          BNE C21TSD
          LDA $83
          CMP #4
          BEQ COM22
C21TSD    LDA #$82                 ;Single Density (SD)-Kennung
          LDX FORKEN2
          BNE FSDATB

C21DD     LDA #0                   ;Double Density (DD)-Kennung

FSDATB    STA FORKEN
          JSR STELL2
          JSR COPSLT               ;Neue Sectorliste in die Zeropage kopieren
          LDA DUMKEN
          AND #$3F
          JSR SETDK
          LDA FORKEN
          AND #1
          EOR #1
          STA MERK2                ;0/1 = Sectorlaenge-Kennung
          LDA #39                  ;40 Tracks
          STA TRACK
FORM1L    JSR FORMTR               ;1 Track formatieren
          BCS FORM1X
          LDA DSPCTR
          AND #$20                 ;Bit 5 (Display+Drive-Kontrollbyte)
          BNE NOVER                ;schaltet Verify beim Formatieren aus
          LDX SECANZ
          JSR RDTRA2               ;alle Sectoren lesen
          BCS FORM1X
          JSR RDTRV2               ;falls Lesefehler 1 Retry
          BCS FORM1X
NOVER     DEC TRACK
          BPL FORM1L
          STZ COMST                ;gesamte Formatierung OK

FORM1X    LDA #$FF
          LDX #0
FORDL     STA EXBUF,X              ;Datenbuffer mit Sector OK-Bits fuellen
          INX
          CPX RWLEN
          BNE FORDL
          BIT COMST
          BPL SFSTAT
          STZ EXBUF                ;bei Formaterror OK-Bits loeschen
          STZ EXBUF+1
          JSR TSTWRP               ;bei Write Protect nicht 'Dumm' schalten
          BNE SFSTAT
          LDA #$80                 ;bei Fornaterror 'Dumm' schalten
          TSB DUMKEN
          JSR DSPD2
SFSTAT    LDX COMPOS               ;bei Befehl aus erweiterter Kommando-Tabelle
          CPX #18                  ;keine Daten an den Computer senden
          BCS CSLX
          JSR QUITT
          JSR SEXBUF               ;Extended Buffer setzen
          LDA SECLEN
          STA RWLEN
          JMP SDBYTS

COPSLT    JSR SDRDDP               ;Drive Density setzen und Anzeigen
          LDA FORKEN
          AND #3
          TAY
          LDX SLTBEG,Y
          LDY #0                   ;aktuelle Sector-Liste in die Zeropage kopieren
CSLTL     LDA SDSTBL,X
          STA SECLST,Y
          INX
          INY
          CPY SECANZ
          BNE CSLTL
CSLX      RTS
SLTBEG    .BYTE DDSTBL-SDSTBL,MDSTBL-SDSTBL,0

;Single Density Sectorliste

SDSTBL .BYTE 1,3,5,7,9,11,13,15,17,2,4,6,8,10,12,14,16,18





;Medium Density Sectorliste

MDSTBL .BYTE 1,3,5,7,9,11,13,15,17,19,21,23,25,2,4,6,8,10,12,14,16,18,20,22,24,26







;Double Density Sectorliste

DDSTBL .BYTE 6,12,18,5,11,17,4,10,16,3,9,15,2,8,14,1,7,13





FORERR    SEC                      ;'Carry'=Format ERROR-Flag
          RTS

;Write Track Kommando starten

FSTART    JSR TRADJA               ;Kopf positionieren und Track # anzeigen
          BNE FORERR
          JSR TSTWRP               ;Write Protect und Klappe pruefen
          BNE FORERR
          LDA #5
          STA $029F
          LDA #$F8                 ;Write Track an Controller geben
          STA $0400
          LDA MERK1
          LDX #2
FORWA1    BIT $0280
          BPL FORWA1
          STA $0403
          DEX
          BNE FORWA1
          LDY #$D0
          STY $029F                ;Timer setzen
          CLC
          RTS

FORMTR    BIT FORKEN
          BPL FORMMD               ;in 'MFM' (MD/DD) formatieren

FORMSD    LDA #0                   ;in 'FM' (Single Density) formatieren
          STA MERK1
          JSR FSTART               ;'Write Track' Kommando starten
          BCS FORERR
          LDY #80
          JSR WRBYTS
          LDA #$FC
          JSR WRBYTE               ;Track AM schreiben

FSDL      TYA
          LDY #25
          JSR WRBYTS

          LDA #$FE
          JSR WRBYTE               ;ID AM
          LDA TRACK
          JSR WRBYTE               ;Track #
          TYA
          JSR WRBYTE               ;Side #
          LDA SECLST,X
          JSR WRBYTE               ;Sector #
          TYA
          JSR WRBYTE               ;Sectorlaenge + 0
          LDA #$F7
          JSR WRBYTE               ;2 CRC-Byte schreiben
          INX

          TYA
          LDY #17
          JSR WRBYTS

          LDA #$FB
          JSR WRBYTE               ;DATA AM
          LDA #$FF
          LDY #$80                 ;128 Bytes
          JSR WRBYTS               ;Datenfeld schreiben
          LDA #$F7
          JSR WRBYTE               ;2 CRC-Bytes schreiben

          CPX SECANZ
          BNE FSDL

FSDEX     LDA #1
FSDEL     AND $0400                ;Warten auf 'In Use'-Flag=0
          BEQ FORM2X
          BIT $0280
          BPL FSDEL
          STY $0403
          BRA FSDEL
FORM2X    CLC                      ;Format Track OK
RTN5      RTS

FORMMD    LDA #$4E                 ;In 'MFM' (MD/DD) formatieren
          STA MERK1
          JSR FSTART               ;'Write Track' Kommando starten
          BCS RTN5
          LDY #$80
          JSR WRBYTS
          TYA
          LDY #12
          JSR WRBYTS
          LDA #$F6
          LDY #3
          JSR WRBYTS
          LDA #$FC
          JSR WRBYTE               ;Track AM schreiben
          LDA #$4E
          LDY #$32
          JSR WRBYTS

FMDL      TYA
          LDY #12
          JSR WRBYTS
          LDA #$F5
          LDY #3
          JSR WRBYTS

          LDA #$FE
          JSR WRBYTE               ;ID AM
          LDA TRACK
          JSR WRBYTE               ;Track #
          TYA
          JSR WRBYTE               ;Side #
          LDA SECLST,X
          JSR WRBYTE               ;Sector #
          LDA MERK2
          JSR WRBYTE               ;Sectorlaenge (0=$80/1=$100)
          LDA #$F7
          JSR WRBYTE               ;2 CRC-Bytes schreiben
          INX

          TYA
          JSR WRBYTE
          LDA #$4E
          LDY #22
          JSR WRBYTS
          TYA
          LDY #12
          JSR WRBYTS
          LDA #$F5
          LDY #3
          JSR WRBYTS

          LDA #$FB
          JSR WRBYTE               ;DATA AM
          LDA #$FF
          LDY SECLEN               ;128 oder 256 Bytes
          JSR WRBYTS               ;Datenfeld schreiben
          LDA #$F7
          JSR WRBYTE               ;2 CRC-Bytes

          CPX SECANZ               ;letzten Sector formatiert ?
          BEQ FMDEX

          LDA #$4E
          LDY #41
          JSR WRBYTS
          BRA FMDL

FMDEX     LDY #$4E
          JMP FSDEX                ;warten auf 'In Use' - Flag = 0

WRBTL     BVC FORTO
WRBYTE    BIT $0280                ;1 Byte an Controller uebergeben
          BPL WRBTL
          STA $0403
          RTS

WRBTSL    BVC FORTO
WRBYTS    BIT $0280                ;Y-Register = Anzahl der an den Controller zu
          BPL WRBTSL               ;uebergebenden Bytes
          STA $0403
          DEY
          BNE WRBYTS
          RTS

FORTO     PLA                      ;Format Time-Out
          PLA
          SEC
          RTS

CLRDSK    LDA #39                  ;* Routine zum 'loeschen' einer kompletten Diskette *
          STA TRACK                ;* Es wird ein unlesbares Format erzeugt *
CLRDKL    JSR CLRTRA
          BCS CLRDKX
          DEC TRACK
          BPL CLRDKL
CLRDKX    RTS

CLRTRA    LDA #$AA                 ;* Einen Track ' loeschen' *
          STA MERK1
          JSR FSTART               ;'Write Track' - Kommando starten
          BCS CLRDKX
CLRLOP    LDA $0400
          LSR                     ;'In Use' - Flag = 0 ?
          BCC CLRDKX
          BIT $0280
          BPL CLRLOP
          STA $0403
          BRA CLRLOP

COM4E     LDX #$75                 ;Diese Routine gibt folgende Werte an den Computer zurueck:
C4EDL     STZ $00,X                ;Tracks/Disk
          INX                      ;Sectoren/Track
          BPL C4EDL                ;Side #
          LDA #$28                 ;FM/MFM-Kennung
          STA $74                  ;Bytes/Sector
          INC $75
          LDA SECANZ
          STA $77
          BIT FORKEN
          BMI SETSEL
          LDA #4                   ;MFM-Kennung
          STA $79
SETSEL    LDA #$80                 ;128 Bytes/Sector
          LDX FORKEN
          BNE LOLEN
          ASL 
          ROL $7A                  ;Sectoren/Track HI
LOLEN     STA $7B                  ;Sectoren/Track LO
          DEC $7C                  ;=FF
          JSR SEND41
          LDA #12
          LDX #$74                 ;12 Bytes ab Adresse $74 an den Computer senden
          LDY #0
          JMP SDBTS

COM4F     LDA #12                  ;* Diese Routine ist das Gegenstueck von COM4E *
          LDX #$74                 ;* Laufwerk konfigurieren                      *
          LDY #0
          JSR RDBTS
          JSR SEND41               ;'A' senden
          LDA #$82                 ;SD-Kennung
          LDX $79
          BEQ C4FSD
          LDA #$41                 ;MD-Kennung
          LDX $77
          CPX #26
          BCS C4FSD
          LDA #0                   ;DD-Kennung
C4FSD     STA FORKEN
          STA FORKEN2
          JSR SDRDDP               ;Drive Density stellen und anzeigen
          JMP SEND43               ;'C' senden

COM53     JSR CONRE2               ;* 'Status' - Kommando *
          LDA DRSTAT               ;Diese Routine gibt folgende Werte an den Computer:
          AND #$F7                 ;Drive Status + Time-Out-Wert
          BIT $0400
          BVC ST7C
          ORA #8                   ;Write Protect Flag
ST7C      STA $7C
          LDA CONST                ;Controller - Status invertieren
          EOR #$FF
          STA $7D
          LDA #$E0
          STA $7E
          STZ $7F
          LDA $0400
          STA CONST
          JSR SEND43               ;'C' senden
          LDA #4
          LDX #$7C                 ;A Bytes ab Adresse $7C
          LDY #0
          JMP SDBTS                ;an den Computer senden


DISP1     = $4000
DISP10    = $4001
DENSITY   = $4002
BUZZER    = $4003

BELL      LDA DSPCTR               ;* Beil-Routine - akustische Unterstuetzung diverser *
          LSR                     ;* Laufwerksfunktionen und Meldungen                 *
          BCC BELLX
BELL1     PHY                      ;Y-Register merken
          LDY #2
BELL2     LDX #0
SO1       LDA #$23
SO2       DEA
          BNE SO2
          STA BUZZER               ;Summer ansprechen
          DEX
          BNE SO1
          DEY
          BNE BELL2
          PLY                      ;Y-Register zurueckholen
BELLX     RTS

CLRDSP    STZ DISP1                ;* Display loeschen *
          STZ DISP10
          STZ DENSITY
          RTS

ANZEIGE   PHA                      ;* Diese Routine wird zur Hex-Darstellung von Werten  *
          PHX                      ;* in Accu auf den Display verwendet, es werden keine *
          PHY                      ;* Register ausser des Status-Registers veraendert    *
          JSR HEXOUT
          PLY
          PLX
          PLA
          RTS

ERRDSP    BIT DSPCTR
          BPL HEXX
HEXOUT    PHA                      ;Hex-Darstel lung auf dem Display
          AND #$0F
          TAX
          LDA SEGTBL,X
          STA DISP1                ;rechtes Display
          PLA
          LSR
          LSR 
          LSR 
          LSR 
          TAX
          LDA SEGTBL,X
          STA DISP10               ;linkes Display
HEXX      RTS

TRAANZ    PHA                      ;* Anzeige der Track # auf dem Display *
          LDA TRACK
          BIT DSPCTR
          BVS TRAHEX
          JSR DEZOUT               ;Dezimal-Darstellung
          PLA
          RTS
TRAHEX    JSR HEXOUT               ;Hex-Darstellung
          PLA
          RTS

DEZOUT    LDX #0                   ;* Wert in Accu in dezimaler Form auf dem Display ausgeben    *
          SEC
DEZ1      TAY
          SBC #10                  ;10'er Stellen abzaehlen
          BMI DEZ2
          INX
          BRA DEZ1
DEZ2      LDA SEGTBL,X
          STA DISP10               ;linkes Display
          LDA SEGTBL,Y
          STA DISP1                ;rechtes Display
	RTS

DENDSP    LDA FORKEN               ;* Drive-Density zur Anzeige bringen *
          AND #3
          TAX
          LDA DENSEG,X             ;Segmenttabeile fuer Density-Anzeige
          STA DENSITY
          RTS

;Segment-Code Tabelle

SEGTBL   .BYTE $3F,6,$5B,$4F,$66,$6D,$7D,7,$7F,$6F,$77,$7C,$39,$5E,$79,$71




;Density-Code Tabelle

DENSEG    .BYTE 4,2,1


BREAK      LDX #$7C                ;* Break-Routine *
           LDY #$50                ;* Sollte der Prozessor auf einen BRK-Befehl stossen, *
           STX DISP10              ;* wird ein 'br' + 2 x Bell ausgegeben und ein        *
           STY DISP1               ;* Sytem-Warmstart ausgefuehrt                        *
           JSR SYSERR
           LDX #$10
           JSR X2WAIT              ;Warteschleife
           JMP RESET2              ;Warmstart


;High-Speed SIO-Routine, wird in relocierter Form zum Computer geschickt

SIO       ; STATUS = $30
          ; CHKSU2 = $31
          ; BUF    = $32
          ; LEN    = $34
          ; CRETRY = $36
          ; DRETRY = $37
          ; STACKP = $3F

          LDA $0301
          BNE SIO2
          LDX #4                   ;bei Laufwerk Nr.=0
DLWTBLL   STA LWTBL-1,X            ;Laufwerk-Tabelle loeschen
          DEX
          BNE DLWTBLL
          RTS
SIO2      TAX
ABS21     LDA LWTBL-1,X            ;Laufwerk schon auf High-Speed gesetzt ?
          BNE SIO3
ABS22     INC LWTBL-1,X
          LDX #7
SIOCL     LDA $0302,X              ;SIO-Kommando retten
          PHA
ABS23     LDA C3F,X
          STA $0302,X              ;COM3F - Tabelle kopieren
          DEX
          BPL SIOCL
          LDA #$31
          STA $0300
          JSR $E459                ;Computer SIO - Routine aufrufen
          LDA #$28                 ;fuer ERROR normale Baudrate setzen
          LDX $0301
          LDY $0303
          BMI SIO21
          LDA $01
SIO21     STA SPTBL-1,X            ;Baud-Rate in Speed-Tabelle eintragen
          LDY #0
SIO21CL   PLA
          STA $0302,Y              ;urspruengliches SIO-Kommando zurueckholen
          INY
          CPY #8
          BCC SIO21CL
SIO3      SEI
          TXA
          ORA #$30                 ;Drive # + Bus ID
          STA $023A
          LDA $0302
          STA $023B                ;SIO - Kommando
          LDA $030A                ;Sector LO
          STA $023C
          LDA $030B                ;Sector HI
          STA $023D
ABS31     LDA SPTBL-1,X
          STA $D204                ;Baud-Rate setzen
          TSX
          STX $3F                  ;Stackpointer retten
          LDA #2
          STA $37                  ;2 Device-Retry's setzen
IO11      LDA #4
          STA $36                  ;4 Command-Retry's setzen
IO12      LDA #$34
          STA $D303                ;Command-Leitung setzen
          LDA #0
          STA $30                  ;Status = 0
          STA $3E
          STA $35
          STA $D206
          LDA #$3A
          STA $32
          LDA #2
          STA $33                  ;Buffer $23A
          ASL                     ;Laenge 4 Bytes
          STA $34
ABS32     JSR SEND1                ;Buffer+Checksumme senden, auf Quittung warten
          LDA $0304
          STA $32                  ;Datenbuffer setzen
          LDA $0305
          STA $33
          LDA $0308
          STA $34
          LDA $0309                ;Datenlaenge setzen
          STA $35
          LDA $0303                ;Daten zum Laufwerk senden ?
          BPL IO2
ABS33     JSR SEND1                ;Datenbuffer+Checksumme senden, auf Quittung warten
IO2       DEC $3E
ABS41     JSR SETTI1               ;Ausfuehrungszeit begrenzen (Time-Out)
          BIT $0303                ;Daten vom Laufwerk uebernehemen ?
          BVC IO3
ABS42     JSR GETA1                ;auf 'C' vom Laufwerk warten
IO3       LDA #$A0
          STA $D207                ;Soundregister zuruecksetzen
          LDA $10
          STA $D20E                ;Pokey-Maske zuruecksetzen
ABS51     JSR CLRTI
          LDA $30
          BEQ IO4                  ;Status OK ?
          DEC $37                  ;Device-Retry abzaehlen
          BNE IO11                 ;und ausfuehren
IO4       TAY
          STY $0303                ;Status setzen
          CLI
          RTS

SEND1     LDY #0
SE1       INY
          BNE SE1
          LDA #$23
ABS61     JSR POKEY                ;Pokey auf senden steiler.
          LDA ($32),Y
          STA $31
          STA $D20D                ;Pokey starten
          INY
          BNE SE3
SE2       LDA ($32),Y
ABS62     JSR PUTBYTE              ;Buffer senden
          INY
          BNE SE3
          INC $33
          DEC $35
          LDX #$E0
SEWL      INX
          BNE SEWL
SE3       CPY $34
          BNE SE2
          LDA $35
          BNE SE2
          LDA $31
ABS63     JSR PUTBYTE              ;Checksummen senden
SEO1      LDA $D20E
          AND #8
          BNE SEO1
          LDY #3
ABS64     JSR STOUTX0              ;Time-Out setzen
          LDA #$C0
          STA $D20E                ;IRQ-Status zuruecksetzen
          BNE RDQUIT

GETA1     LDY #0
          STY $31
GE1       JSR GETBYTE              ;Datenblock vom Laufwerk holen
          STA ($32),Y
ABS71     JSR ADDSUM
          INY
          BNE GE2
          INC $33
          DEC $35
GE2       CPY $34
          BNE GE1
          LDA $35
          BNE GE1
ABS72     JSR GETBYTE              ;Cheksumme holen
          CMP $31
          BNE ERR8F
          RTS 

IOER80    LDA #$80                 ;Break-Status
          STA $30
          LDX $3F
          TXS                      ;Stackpointer zuruecksetzen
EABS0     JMP IO3
ERR8F     LDA #$8F                 ;Checksummen - Error
          .BYTE $2C
ERR8A     LDA #$8A                 ;Time-Out - Error
ERROR     STA $30
          LDX $3F
          TXS                      ;Stackpointer zuruecksetzen
          LDA $3E
          BMI ERRA
          DEC $36
          BEQ ERRA
EABS1     JMP IO12
ERRA      LDA #$28
          STA $D204                ;Baudrate normal setzen
          LDA #0
          LDX $0301
EABS2     STA LWTBL-1,X            ;Eintrag in Laufwerktabelle loeschen
EABS3     JMP IO3

SETTI1    LDX #1
          LDY #$5E
ABS81     JSR STOUT                ;Time-Out fuer Datenfeld setzen

RDQUIT    LDA #$3C
          STA $D303
          LDA #$13
RDA1      JSR POKEY                ;Pokey auf lesen stellen
RDA2      JSR GETBYTE              ;Quittung holen
          CMP #$41                 ;'A'
          BEQ CLRTI
          CMP #$43                 ;'C'
          BEQ CLRTI
          CMP #$45                 ;'E'
          BEQ ERR90
          LDA #$8B                 ;Drive not ready
          BNE ERROR
ERR90     LDA #$90
          STA $30                  ;Status setzen

CLRTI     LDY #0                   ;Timer loeschen
STOUTX0   LDX #0
STOUT     LDA ERRABS
          STA $0226                ;Timer 1 Sprungvector setzen
STO2      LDA ERRABS+1
          STA $0227
          LDA #1
          JMP $E45C
ERRABS    .WORD ERR8A


GETBYTE   LDA $D20E                ;1 Byte vom Pokey holen
JMPE80    BPL IOER80
          AND #$20                 ;auf 'Shiftregister voll' warten
          BNE GETBYTE
          LDA #$DF
          STA $D20E                ;IRQ - Flag zuruecksetzen
          LDA #$E0
          STA $D20E                ;IRQ - Status neu setzen
          LDA $D20F
          STA $D20A
          BPL ERR8A
          AND #$20
          BEQ ERR8A
          LDA $D20D                ;Shift - Register uebernehmen
          RTS

PUTBYTE   PHA                      ;1 Byte an den Pokey uebergeben
PUTA1     LDA $D20E
          AND #$10                 ;Warten auf 'Shift - Register leer'
          BNE PUTA1
          LDA #$EF
          STA $D20E                ;IRQ - Flag zuruecksetzen
          LDA #$D0
          STA $D20E                ;IRQ - Status neu setzen
          PLA
          STA $D20D                ;Byte an Shift - Register uebergeben
          LDX $D20E
          BPL JMPE80


ADDSUM    CLC                      ;Checksumme errechnen
          ADC $31
          ADC #0
          STA $31
          RTS

POKEY     STA $D20F                ;Pokey fuer Datenein- und ausgabe vorbereiten
          STA $D20A
          LDA #$28
          STA $D208
          LDA #$A8
          STA $D207                ;Sound - Register vorbereiten
          LDA #$F8
          STA $D20E                ;IRQ - Enable setzen
          RTS

;COM3F - Tabelle

C3F      .BYTE 63,64,1,0,1,0,1,0

SPTBL    .BYTE 40,40,40,40        ;Baudraten - Tabelle fuer Laufwerke 1-4
LWTBL    .BYTE 0,0,0,0            ;Pruef - Tabelle fuer Laufwerke 1-4

SIOEND

;Absolute Adressen Tabelle fuer Relocator

ABSTBL    .WORD DLWTBLL+1,ABS21+1,ABS22+1,ABS23+1,SIO21+1,ABS31+1





          .WORD ABS32+1,ABS33+1,ABS41+1,ABS42+1,ABS51+1,ABS61+1,ABS62+1






          .WORD ABS63+1,ABS64+1,GE1+1,ABS71+1,ABS72+1,EABS0+1,EABS1+1,EABS2+1







          .WORD EABS3+1,ABS81+1,RDA1+1,RDA2+1,STOUT+1,STO2+1,ERRABS







;Laufwerks - Testroutinen (werden in spaeteren Versionen erweitert)

ROMTST    JSR STELL2               ;* ROM-Testroutine *
          LDX #0
          LDA #$E0
          STA IND+1                ;ROM - Adresse $E000
          LDY #0
          STY IND
ROMT2     TYA
          CLC
ROMTL     ADC (IND),Y              ;Checksumme fuer 1 ROM-Page errechnen
          INY
          BNE ROMTL
          CMP ROMCHK,X             ;Checksumme OK
          BNE ROMTX
          INC IND+1
          INX
          CPX #$20                 ;32 Pages getestet ?
          BCC ROMT2
          STZ COMST                ;OK - Status setzen
ROMTX     JMP QUITT

RAMTST    JSR STELL2               ;* RAM-Testroutine *
          LDA #$55
          STA EXBUF
          LDY #0
RT1L      LDA $00,Y
          STA EXBUF+1
          LDA EXBUF
          STA $00,Y
          CMP $00,Y                ;Zeropage testen
          BNE ZPERR
          LDA EXBUF+1
          STA $00,Y
          INY
          BNE RT1L
          ASL EXBUF
          BCC RT1L

          LDA #$80
          STA $91                  ;RAM - Adresse $8000 setzen
          LDY #0
          STY $90
RTABSL    LDA #$55
          STA MERK1
RT2L      LDA ($90),Y
          STA MERK2
          LDA MERK1
          STA ($90),Y
          CMP ($90),Y              ;RAM von $8000 bis RAMTOP testen
          BNE RTERR
          LDA MERK2
          STA ($90),Y
          INY
          BNE RT2L
          ASL MERK1
          BCC RT2L
          INC $91
          LDA $91
          CMP # >(CMTBL+$0100)       ;RAMTOP erreicht ?
          BCC RTABSL
          BRA SPCAX

ZPERR     STZ $91                  ;fehlerhafte RAM-Adresse zum senden zwischenspeichern
RTERR     STY $90
          BRA SPTEX

SPEEDT    JSR STELL2               ;* Motor - Speed - Test *
          STZ TRACK
          JSR TRADJA               ;Track 0 positionieren
          JSR FDSEC1               ;Abstand zwischen SEctor 1 und Sector 1 testen
          BCS SPTEX
          JSR FDSEC1
          BCS SPTEX

          STZ $90
          STZ $91                  ;Counter loeschen
          LDA #$E4
          STA $92
          LDA #$E1
          STA $93
          LDA #$C0                 ;von Konstante $E4E1C0 die gezaehlten Taktzyklen abzaehlen
          STA $94
SPCAL     LDA $94
          SEC
          SBC MERK2
          STA $94
          LDA $93
          SBC MERK3
          STA $93
          BCS SPCAD
          LDA $92
          SBC #0
          STA $92
          BCC SPCEND
SPCAD     JSR SPCADD
          BRA SPCAL

SPCADD    SED                      ;Counter in Dezimalmodus heraufzaehlen
          LDA $91
          CLC
          ADC #1
          STA $91
          LDA $90
          ADC #0
          STA $90
          CLD
          RTS

SPCEND    LDA $94
          EOR #$FF
          STA $94
          LDA $93
          EOR #$FF
          STA $93
          LSR MERK3
          LDA MERK2                ;eine Stelle hinter dem Komma runden
          ROR 
          SEC
          SBC $94
          LDA MERK3
          SBC $93
          BCC SPCAX
          JSR SPCADD

SPCAX     STZ COMST
SPTEX     JSR QUITT                ;Speed-Wert zum Computer senden
          LDA #2
          LDX #$90
          LDY #0
          JMP SDBTS

FDSEC1  LDA #1
          STA $0402                ;Sector # in Sector-Register des Controllers setzen
          LDA #$88
          STA $0400
          LDA #$D8
          STA $029F                ;Time-Out setzen
          STZ MERK2
          STZ MERK3                ;Counter zuruecksetzen
          LDX #7
FDS1WL    DEX
          BNE FDS1WL
FDS1L     BIT $0280
          BVC FDS1TO
          BMI FDS1DR
          INC MERK2
          BNE FDS1NI
          INC MERK3
          BRA FDS1TZ
FDS1NI    PHA                      ;Zeit nach Maschinenzyklen (1 Mhz) festlegen
          PLA
FDS1TZ    PHA
          PLA
          PHA
          PLA
          BRA FDS1L

FDS1DR    CLC
          JMP CONRES

FDS1TO    SEC                      ;Time-Out - Error Kennzeichnen
          JMP CONRE2


;Freier ROM-Speicher fuer zukuenftige Erweiterungen

;          *= $FE00
	org $fe00

COM3F     JSR SEND41
          LDA #$09                 ;High-Speed Wert fuer Pokey an den Computer senden
          JSR SDBYTE
          LDA #$09
          JSR SDBYTE
          LDA #1
          STA USKEN                ;Datenuebertragung auf High-Speed stellen
          RTS

NORDB     BIT $0280                ;* Read-Byte Routine fuer normale Uebertragungsrate *
          BVC RDBTO
          BIT $0282
          BVC NORDB                ;auf Startbit warten
          LDX #6
RDBL1     DEX
          BNE RDBL1
          LDA #$80
RDNBIT    LDX #7
RDBL2     DEX
          BNE RDBL2
          BIT $0282                ;1 Bit uebernehmen
          BVC SETC
          CLC
          BCC RDBSB
SETC      SEC
          NOP
RDBSB     ROR 
          BCC RDNBIT
          RTS

RDBTO     PLA                      ;Ruecksprungadresse vom Stack holen
          PLA
          JMP ER40UK               ;Kennung fuer Uebertragungsrate umschalten

USRDB     BIT $0282
          BVC USRDB
          LDX #8
USBITL    LDA $0282                ;* 1 Datenblock in High-Speed von Computer holen *
          ASL 
          ASL 
          ROR MERK1
          DEX
          BPL USBITL
SVSDB     TXA
          EOR MERK1
USW1      BIT $0282                ;1 Bit uebernehmen
          BVC USW1
          STA (IND),Y
          CLC
          ADC CHKSUM               ;Checksumme heraufzaehlen
          ADC #0
          STA CHKSUM
          LDX #7                   ;Bit - Zaehler fuer 8 Bits
USW2      LDA $0282
          ASL 
          ASL 
          ROR MERK1
          DEX
          BPL USW2
          INY
          CPY RWLEN                ;Checksumme empfangen ?
          BNE SVSDB
USRDBX    PLA
          PLA
          TXA
          EOR MERK1
          JMP RDEXIT

NOSDB     LDA #1                   ;* 1 Byte in Normal Speed an den Computer senden *
          TRB $0282                ;Startbit setzen
          PHX
          PHY
          LDY #8                   ;8 Bit Zaehler
          LDX #5
SBWL1     DEX
          BNE SBWL1

SDBITL    LDA $0282
          LSR 
          LSR MERK1                ;1 Bit ins PIO-Register shiften
          ROL 
          STA $0282
          LDX #5
SBWL2     DEX
          BNE SBWL2
          NOP
          NOP
          DEY
          BNE SDBITL               ;alle Bits gesendet

          ORA #1
          STA $0282                ;Stopbit setzen

          LDX #3
SBWL3     DEX
          BNE SBWL3
          PLY
          PLX
          RTS

USSDB     LDA $0282
          AND #$FE                 ;Startbit setzen
          STA $0282
          LDX #8                   ;Bitzaehler
USSBL     LSR 
          ROR MERK1
          ROL                     ;1 Bit ins PIO-Register shiften
          STA $0282
          DEX
          BNE USSBL
          LSR 
          SEC                      ;Stopbit setzen
	ROL
          CMP $00
          STA $0282
          RTS


;          *= $FEE0
	org $fee0

;Checksummenbytes fuer Romtest
;(werden bei'm assemblieren nicht richtig gesetzt

ROMCHK   .BYTE 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16



          .BYTE 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32





;Vektortabelle
;Von eigenen Programmen sollte nur ueber diese Vectortabelle gesprungen werden,
;da die Programme sonst auf den folgenden Speedy-Versionen nicht laufen koennten.

;          *= $FF00
	org $ff00

          JMP RESET                ;Drive Kaltstart
          JMP RESET2               ;Warmstart
          JMP BEREIT               ;Bereitschaftroutine
          JMP MOTON                ;Motor zwingend einschalten
          JMP TSTMON               ;Motor einschalten wenn Klappe zu ist
          JMP MOTOFF               ;Motor ausschalten
          JMP SDELAY               ;Motor Timer stellen
          JMP SDRDDP               ;Drive Density stellen und anzeigen
          JMP XWAIT                ;Warteschleife kurz
          JMP X2WAIT               ;Warteschleife lang
          JMP TRACK0               ;Track 0 positionieren
          JMP TRADJA               ;Track # anzeigen und Kopf positionieren
          JMP TRADJ                ;Kopf positionieren, Track # nur anzeigen, wenn Trackwechsel
          JMP TRVR                 ;1 Step vorwaerts oder rueckwaerts gehen
          JMP CONRES               ;Disk Controller stoppen
          JMP CONRE2               ;2 mal CONRES
          JMP WREADY               ;auf Controller 'In Use'-Flag=0 warten
          JMP RD128B               ;128 Bytes vom Computer nach EXBUF holen
          JMP RD256B               ;256 Bytes vom Computer nach EXBUF holen
          JMP RDBTS                ;Accu=Anzahl der Bytes nach Buffer (X/Y-Register) holen
          JMP RDSFOL               ;nach Verzoegerung Sectorfolge vom aktuellen Track lesen
          JMP RDSFO1               ;sofort Sectorfolge vom aktuellen Track lesen
          JMP RDTRA                ;alle Sectoren des aktuellen Track ins RAM einiesen
          JMP RDTRAV               ;wie RDTRA aber mit Verify und einem Retry   
          JMP TSTWR                ;noch zu schreibende Sectoren aus RAM auf Diskette schreiben
          JMP TSTDAT               ;TSTWR ausfuehren und alle Sectoren als nicht gelesen markieren
          JMP SD128B               ;128 Bytes vom EXBUF zum Computer senden
          JMP SD256B               ;256 Bytes vom EXBUF zum Computer senden
          JMP SDBTS                ;Accu=Anzahl der Bytes aus Buffer (X/Y-Reg.) senden
          JMP SEND41               ;'A' zum Computer senden
          JMP SEND43               ;'C' zum Computer senden
          JMP SEND45               ;'E' zum Computer senden
          JMP SEND4E               ;'N' zum Computer senden
          JMP RDSECT               ;aktuellen Sector von Diskette in vorbezeichneten RAM einlesen
          JMP RDSEC1               ;bezeichneten Sector in bezeichneten RAM einlesen
          JMP WRSECT               ;aktuellen Sector von vorbezeichneter RAM-Adr. auf Disk schreiben
          JMP WRSEC1               ;bezeichnten Sector von vorbezeichneter RAM-Adr. schreiben
          JMP TSTWRP               ;Write Protect und Klappe testen
          JMP VERSEC               ;aktuellen Sector mit angegebenem RAM vergleichen
          JMP VERSE1               ;bezeichenten Sector mit angegebenem RAM vergleichen
          JMP STELL                ;COM-Status auf 'Error' und 2 Retry's setzen
          JMP QUITT                ;Quittung 'C' oder 'E' je nach COM-Status senden
          JMP RDHEAD               ;Die nachsten 'Header'-Daten lesen
          JMP RDHD1                ;wie RDHEAD aber Timer nicht setzen
          JMP RDHDSP               ;Kopf positionieren und nachsten 'Header' lesen
          JMP CALCTS               ;Track- und Sectornummer errechnen
          JMP SETBUF               ;Buffer nach aktuellem Sector setzen
          JMP SETBUF2              ;Buffer nach Sectornummer im Accu setzen
          JMP SEXBUF               ;Adresse des Extended-Buffers setzen
          JMP SETRWL               ;Anzahl der Bytes fuer zu uebertragenden Datenblock setzen
          JMP COPSLT               ;Sectorliste fuer aktuelles Density in Zeropage kopieren
          JMP BELL1                ;1 Bell (Buzzer) ausgeben
          JMP CLRDSP               ;Display abschalten
          JMP TRAANZ               ;aktuelle Track # anzeigen
          JMP DEZOUT               ;Wert in Accu in dezimaler Form anzeigen
          JMP HEXOUT               ;Wert im Accu in Hexadezimaler Form anzeigen
          JMP DENDSP               ;aktuelles Density anzeigen
          JMP SETTIM               ;Timer mit Wert im Accu setzen
          JMP CLRTRA               ;Einen Track mit unlesbarem Format versehen (reformatieren)
          JMP CLRDSK               ;ganze Diskette reformatieren
          JMP RAMTST               ;Einsprung fuer RAM-Test, 2 Bytes werden gesendet
          JMP ROMTST               ;ROM-Test Einsprung, es wird nur quittiert
          JMP SPEEDT               ;Einsprung fuer Speed-Test, 2 Bytes werden gesendet


;          *=$FFF8
	org $fffb

          .BYTE VERSION            ;Speedy Verions Nummer
          .WORD RESET              ;Reset - Vektor fuer den Prozessor
          .WORD BREAK              ;Break - Vektor fuer den Prozessor

	end

