 IDENTIFICATION DIVISION.
 PROGRAM-ID. MENU.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
       COPY MBMS.
       COPY DFHAID.
       COPY DFHBMSCA.
 01  WS-MESSAGE     PIC X(30) VALUE 'THANK YOU'.
 01  WS-CA.
     03 WS-CUS-ACCNO  PIC 9(15) VALUE ZERO. 
 LINKAGE SECTION.      
 01  DFHCOMMAREA    PIC X(16). 
 PROCEDURE DIVISION.
 MAIN-PARA.                                                       
       IF EIBCALEN = ZERO                                           
          PERFORM ERROR-PARA                                       
       ELSE                                                         
          MOVE DFHCOMMAREA TO WS-CA                                 
          PERFORM SHOW-MAP-PARA                                         
      END-IF.                                                      
 END-PARA.                                                        
     EXEC CICS RETURN                                             
         TRANSID('P37W')                                          
         COMMAREA(WS-CA)                                          
     END-EXEC.                                                    
*SHOW ERROR IF EIBCALEN=ZERO     
 ERROR-PARA.    
       EXEC CICS SEND TEXT
           FROM(WS-MEASSAGE)
           ERASE
       END-EXEC 
       EXEC CICS RETURN 
       END-EXEC.
*SHOW MAP IF EIBCALEN IS NOT ZERO      
 SHOW-MAP-PARA.                                                     
       MOVE LOW-VALUES TO MENMAPO                                   
*SEND APT MAP(MENU)        
       EXEC CICS SEND                                               
           MAP('MENMAP')                                            
           MAPSET('MBMS')                                        
           FROM(MENMAPO)                                            
           ERASE                                                    
       END-EXEC      
       PERFORM RESPONSE-PARA.
*CHECK ENTERED KEY       
 RESPONSE-PARA.                                                    
       EVALUATE EIBAID                                              
       WHEN DFHESC                                                  
           PERFORM ESC-PARA                                         
       WHEN DFHPF3                                                  
           PERFORM PF3-PARA       
       WHEN DFHENTER                                                
           PERFORM OPTION-PARA                                 
       WHEN OTHER                                                   
           MOVE 'INVALID KEY PRESSED' TO MSGO                       
       END-EVALUATE.                                                
 ESC-PARA.      
       EXEC CICS RETURN                                             
           TRANSID('P37W')                                          
           COMMAREA(WS-CA)                                          
       END-EXEC. 
 PF3-PARA.      
       EXEC CICS XCTL
           PROGRAM('BACKPGM')
       END-EXEC
 OPTION-PARA.
       EVALUATE CHOICE 
       WHEN 1
           EXEC CICS XCTL
               PROGRAM('WITHDRAW')
           END-EXEC
       WHEN 2
           EXEC CICS XCTL
               PROGRAM('DEPOSIT')
           END-EXEC
       WHEN 3
           EXEC CICS XCTL
               PROGRAM('MINISTMT')
           END-EXEC
       WHEN 4
           EXEC CICS XCTL
               PROGRAM('ENQUERY')
           END-EXEC
       WHEN OTHER
           MOVE 'INVALID OPTION! PLEASE ENTER VALID OPTION' MSGO
       END-EVALUATE.
            
