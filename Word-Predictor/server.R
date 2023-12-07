#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# loading the data
load("D:/R/Data-Science-Specialization-Capstone-Project/Model.RData")

# bigram_mle()
bigram_mle <- function(D,u.model,b.mle,flag=0) {
    D[!(D %in% rownames(u.model))] <- "UNK"; 
    L <- length(D)
    if(flag == 0) { 
        Y <- D[1:(L-1)]
        Z <- D[2:L]
    } else { # special case for predicted word
        Z <- rownames(u.model); Z <- Z[1:(length(Z)-1)] # Map against dictionary  
        Y <- rep(D[L],length(Z))
    }
    YZ <- paste(Y,Z)
    id1 <- (YZ %in% rownames(bigram.model))
    id2 <- (!id1) & (Y != "UNK")
    id3 <- (!id1) & (Y == "UNK")
    a <- matrix(NA,length(YZ),1)
    if(sum(id1)>0) { a[id1] <- bigram.model[YZ[id1],2] }
    if(sum(id2)>0) { a[id2] <- log(1/u.model[Y[id2],1]) }
    if(sum(id3)>0) { a[id3] <- u.model["UNK",2] }
    rownames(a) <- YZ; colnames(a) <- "MLE"
    a
}

# trigram_mle()
trigram_mle <- function(D,u.model,b.model,t.model,flag = 0) {
    D[!(D %in% rownames(u.model))] <- "UNK"; L <- length(D)
    if(flag == 0) {
        P <- D[1:(L-2)]; Q <- D[2:(L-1)]; R <- D[3:L]
    }else { # special case for predicted word
        R <- rownames(u.model); R <- R[1:(length(R)-1)] # Map against dictionary
        P <- rep(D[L-1],length(R)) 
        Q <- rep(D[L],length(R))
    }
    PQR <- paste(P,Q,R)
    id1 <- (PQR %in% rownames(t.model))
    PQ <- paste(P,Q)
    id2 <- (!id1) & (PQ %in% rownames(b.model))
    id3 <- (!id1) & (!(PQ %in% rownames(b.model))) & (P != "UNK")
    id4 <- (!id1) & (!(PQ %in% rownames(b.model))) & (P == "UNK")
    
    a <- matrix(NA,length(PQR),1)
    if(sum(id1)>0) { a[id1] <- t.model[PQR[id1],2] }
    if(sum(id2)>0) { a[id2] <- log(1/b.model[PQ[id2],1]) }
    if(sum(id3)>0) { a[id3] <- log(1/u.model[P[id3],1]) }
    if(sum(id4)>0) { a[id4] <- u.model["UNK",2] }
    
    rownames(a) <- PQR; colnames(a) <- "MLE"
    a
}

# ngram_predict()
ngram_predict <- function(data) {
    
    D <- ngram_tokenize(data) # Tokenize
    D <- unlist(D); D[!(D %in% names(unigram.mle))] <- "UNK" # mark the ones not in dictionary as UNK
    L <- length(D)
    
    if(L==0) { P <- "UNK"; Q <- "UNK"}
    if(L==1) { P <- "UNK"; Q <- D[L] }
    if(L>1) { P <- D[L-1]; Q <- D[L] }
    R <- names(unigram.mle); R <- R[-length(R)] # Map against dictionary
    
    PQR <- paste(P,Q,R)
    id1 <- (PQR %in% names(trigram.mle))
    PQ <- paste(P,Q)
    QR <- paste(Q,R)
    id2 <- (!id1) & (PQ %in% names(bigram.mle))    &   (QR %in% names(bigram.mle))
    id3 <- (!id1) & (PQ %in% names(bigram.mle))    & (!(QR %in% names(bigram.mle)))
    id4 <- (!id1) & (!(PQ %in% names(bigram.mle))) &   (QR %in% names(bigram.mle))
    id5 <- (!id1) & (!(PQ %in% names(bigram.mle))) & (!(QR %in% names(bigram.mle)))
    
    a <- matrix(NA,length(PQR),1)
    if(sum(id1)>0) { a[id1] <- trigram.mle[PQR[id1]] }
    if(sum(id2)>0) { a[id2] <- bigram.mle[PQ] + bigram.mle[QR[id2]]            + log(2*0.4) }
    if(sum(id3)>0) { a[id3] <- bigram.mle[PQ] + unigram.mle[Q] + unigram.mle[R[id3]]  + log(3*0.4) }
    if(sum(id4)>0) { a[id4] <- unigram.mle[P]  + unigram.mle[Q] + bigram.mle[QR[id4]] + log(3*0.4) }
    if(sum(id5)>0) { a[id5] <- unigram.mle[P]  + unigram.mle[Q] + unigram.mle[R[id5]]  + log(4*0.4) }
    
    pred_word <- R[which.max(a)]
}

# Define server logic required to draw a histogram
function(input, output) {
   
    observeEvent(input$predictBtn, {
        phrase <- isolate(input$input_text)
        if (nchar(phrase) > 0) {
            predicted_words <- ngram_predict(phrase)
            output$predictionText <- renderText({
                paste("Predicted word:", paste(predicted_words, collapse = ", "))
            })
        }
         
        })
}
