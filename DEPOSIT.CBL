 IDENTIFICATION DIVISION.
 PROGRAM-ID. DEPOSIT.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
       COPY WDBMS.
       COPY DFHAID.
       COPY DFHBMSCA.
 01  WS-AMOUNT      PIC 9(05) VALUE ZERO.      
 01  WS-DONE        PIC X(01) VALUE 'N'.
 01  WS-MESSAGE     PIC X(30) VALUE 'THANK YOU'.
 01  WS-CA.
     03 WS-CUS-ACCNO  PIC 9(15) VALUE ZERO. 
 LINKAGE SECTION.      
 01  DFHCOMMAREA    PIC X(21). 
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
       MOVE LOW-VALUES TO APTMAPO                                   
*SEND APT MAP(DEPOSIT)        
       EXEC CICS SEND                                               
           MAP('APTMAP')                                            
           MAPSET('DPBMS')                                        
           FROM(APTMAPO)                                            
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
           PERFORM CALCULATION-PARA                                 
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
 CALCULATION-PARA.                                                
       EXEC CICS RECEIVE                                            
           MAP('APTMAP')                                            
           MAPSET('DPBMS')                                        
           INTO(APTMAPI)                                            
       END-EXEC                                                     
       MOVE ENTERED-AMT TO WS-AMOUNT
       IF WS-AMOUNT < 100 OR WS-AMOUNT > 40000                      
           MOVE 'PLEASE ENTER AMOUNT > 40000 AND < 100' TO MSGO.
       ELSE                                                         
           PERFORM TRANSACTION-PARA                                 
       END-IF.               
 TRANSACTION-PARA.                                                
       PERFORM READ-FILE                                            
 READ-FILE.    
       EXEC CICS READ        
           FILE('P37AFILE')  
           RIDFLD(WS-CUS-ACCNO) 
           INTO(WS-RECORD)   
           RESP(WS-RESP-CODE)
       END-EXEC             
       EVALUATE WS-RESP-CODE
       WHEN DFHRESP(NORMAL)
           PERFORM RETRIEVE-BAL 
           IF WS-DONE = 'Y'
               MOVE 'DEPOSIT SUCESSFULL' TO MSGO 
               PERFORM NEXT-PAGE              
           ELSE
               MOVE 'DEPOSIT NOT SUCESSFULL' TO MSGO
           END-IF
       WHEN OTHER
           MOVE 'DEPOSIT NOT SUCESSFULL' TO MSGO
       END-EVALUATE.
 RETRIEVE-BAL.     
       MOVE CUS-BALANCE TO WS-BALANCE                                
       COMPUTE WS-BALANCE = WS-BALANCE + WS-AMOUNT
       PERFORM UPDATE-BAL.
 UPDATE-BAL.    
       EXEC CICS READ            
           FILE('P37AFILE')      
           RIDFLD(WS-CUS-ACCNO)     
           INTO(WS-RECORD)       
           RESP(WS-RESP-CODE)    
           UPDATE                
       END-EXEC                  
       EVALUATE WS-RESP-CODE
       WHEN DFHRESP(NORMAL)
           EXEC CICS REWRITE        
               FILE('P37AFILE')   
               RIDFLD(WS-CUS-ACCNO)  
               FROM(WS-RECORD)    
               RESP(WS-RESP-CODE) 
           END-EXEC               
           EVALUATE WS-RESP-CODE
           WHEN DFHRESP(NORMAL)
               MOVE WS-BALANCE TO CUS-BALANCE 
               MOVE 'Y' TO WS-DONE
           WHEN OTHER
               MOVE 'UPDATE NOT SUCESSFULL' TO MSGO
           END-EVALUATE
       WHEN OTHER
           MOVE 'UPDATE NOT SUCESSFULL' TO MSGO
       END-EVALUATE.    
 NEXT-PAGE.
       EXEC CICS XCTL          
           PROGRAM('RECEIPT')  
       END-EXEC.                