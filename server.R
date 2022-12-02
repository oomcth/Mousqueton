#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shinyTime)
library(shiny)
library(dplyr)
library(neo4r)
library(igraph)
library(stringr)
library(visNetwork)
library(tidyverse)
library(etherscanr)
library(DT)
con = neo4r::neo4j_api$new(
  url = "http://34.79.35.240:7474",
  user = 'neo4j',
  password = "azerty5"
)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  
  
  couleur = function(x){ if (x == 'Création de contrat') {return ("red")} else if (x=="Transaction") {return ("blue")} else {return ("green")}}
  degre  = function(node_Hash){
    S= 0
    for (val in G$relationships$to){
      if ( val ==node_Hash )
      {
        S=S+1
      }
    }
    for (val in G$relationships$from){
      if ( val ==node_Hash )
      {
        S=S+1
      }
    }
    return (S)
  }
  tronquer = function(x){
    return (str_sub(x,start = 1 , end = 6))
  }

  titriser = function(x){
    S= paste("<b>Time stamp :</b> ", x$Time_stamp, ", <b>Valeur</b>  : " , x$value , ",<br> <b>Est un smart contract :</b> ",x$Est_un_smart_contract, ", <b>Etat</b> : ", x$État , ",<br> <b>Hash :</b> ", x$Hash, ",<br> <b>Gas :</b> ", x$Gas, ", <b>Gas price :</b> ", x$Gas_price , ',<br> <a href="https://etherscan.io/tx/',x$Hash,'">Lien Etherscan</a>', sep = "")
  }

  get_balance <- function(adress, key){
    etherscan_balance(adress, key) ['balance']
  }
  get_histo <- function(adress, key)
  {
    etherscan_transactions(adress, startblock = 0,
                           endblock = 999999999, offset = 1000, page = 1,
                           key)
  }
  
  
  observeEvent(input$do,{
    observeEvent(input$mygraph_selected, {
      updateTextInput(session,"address",value = filter(G$nodes, id == input$mygraph_selected)$Hash)
})
  if( length(input$etat)==1){
  G = paste("MATCH(a:Address{Hash:'",input$address,"'})-[c:SEND]->(b:Address)WHERE c.Est_un_smart_contract= '",input$type,"' and c.Valeur>= ",input$min," and c.Valeur <= ", input$max," and c.État = '",input$etat,"'  and '", input$daterange[1] , "' <= left(c.Time_stamp,10) <= '",input$daterange[2]  ,"' return c UNION ALL MATCH(a:Address)-[c:SEND]->(b:Address{Hash:'",input$address,"'})WHERE c.Est_un_smart_contract= '",input$type,"' and c.Valeur>= ",input$min," and c.Valeur <= ", input$max, ," and c.État = '",input$etat,"' and '", input$daterange[1] , "' <=  left(c.Time_stamp,10) <= '" , input$daterange[2]   , "' return c limit 100", sep = "" )}
    else{
      G = paste("MATCH(a:Address{Hash:'",input$address,"'})-[c:SEND]->(b:Address)WHERE c.Est_un_smart_contract= '",input$type,"' and c.Valeur>= ",input$min," and c.Valeur <= ", input$max,"  and '", input$daterange[1] , "' <= left(c.Time_stamp,10) <= '",input$daterange[2]  ,"' return c UNION ALL MATCH(a:Address)-[c:SEND]->(b:Address{Hash:'",input$address,"'})WHERE c.Est_un_smart_contract= '",input$type,"' and c.Valeur>= ",input$min," and c.Valeur <= ", input$max," and '", input$daterange[1] , "' <=  left(c.Time_stamp,10) <= '" , input$daterange[2]   , "' return c limit 100", sep = "" )
      
      }



    G =G %>%  call_neo4j(con, type = "graph")
    
   

    G$nodes <-  G$nodes %>%unnest_nodes()
    
    
    G$relationships <- G$relationships %>%  unnest_relationships()
    G$nodes$label = lapply(G$nodes$Hash,tronquer)
    G$relationships  = G$relationships %>%rename(from = startNode, to = endNode, label = Valeur   ,value = Valeur  )
    G$relationships$label = G$relationships$value
    G$relationships$color = lapply(G$relationships$Est_un_smart_contract,couleur)
    
    G$nodes$value = lapply(G$nodes$id,degre)
    G$nodes$title = paste ("<b>Adresse : </b>" , G$nodes$Hash, ',<br>  <a href="https://etherscan.io/address/',G$nodes$Hash,'">Lien Etherscan</a>', sep ="")
    
    G$relationships$title = titriser(G$relationships)
    visNetwork(G$nodes, G$relationships) %>% visNodes (scaling = list(min = 10 , max = 50))
    output$mytable = renderTable(tail(select(get_histo(input$address, "XWGEJ8V7NI53IKIJUGDMRXS1Y1KVAFGVSP"), c(2,3,7,8,9) ) , 10 ),title="Historique des transactions")
    output$mygraph <- renderVisNetwork({visNetwork(G$nodes, G$relationships,main = "Visualisation graphique")%>% visOptions (nodesIdSelection = TRUE)  %>% visEdges(arrows = 'to') %>% visNodes (scaling = list(min = 10 , max = 50)) %>%  visPhysics(solver = "forceAtlas2Based")})
    output$mytext = renderText({paste("Balance : ", get_balance (input$address, "XWGEJ8V7NI53IKIJUGDMRXS1Y1KVAFGVSP"))})
    observeEvent(input$button,{write_csv(x= tail(select(get_histo(input$address, "XWGEJ8V7NI53IKIJUGDMRXS1Y1KVAFGVSP"), c(2,3,7,8,9) ) , 10 ), "export.csv")})
    })




  
  
})
