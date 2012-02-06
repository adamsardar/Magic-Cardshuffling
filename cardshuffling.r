# A module full of useful shuffles. These shall be analysed to test which performs best with real magic data.

Bayer.Diaconis.coefficients <- function(n){
  
  BD.dist<-choose(n,0:n)*(1/2)^n
  #Yes I could use rbinom, but I'm trying o learn R properly ...
  
  return(BD.dist)
}

Bayer.Diaconis.split<-function(decksize,prob=0.5){

  #split.at = sample(0:decksize,1,replace=TRUE,prob=Bayer.Diaconis.coefficients(decksize))

  split.at<-rbinom(1,decksize,prob) #Stupidly, I didn't realise that these were the same thing!
  return(split.at)
}


split.deck<-function(deck,prob=0.5){
  
  deck.size<-length(deck)
  cut.point<-Bayer.Diaconis.split(deck.size,prob)
  
  if(cut.point == 0){
    
    deck.cut.two.indicies<-seq(1,deck.size)
    
    cut.one<-c()
    cut.two<-deck[deck.cut.two.indicies]
    
  }else if(cut.point == deck.size){
    
    deck.cut.one.indicies<-seq(1,deck.size)
    
    cut.one<-deck[deck.cut.one.indicies]
    cut.two<-c()
        
  }else{
    
    deck.cut.one.indicies<-seq(1,cut.point)
    deck.cut.two.indicies<-seq(cut.point+1,deck.size)
    
    cut.one<-deck[deck.cut.one.indicies]
    cut.two<-deck[deck.cut.two.indicies]
  }

  cuts<-list(cut.one,cut.two)
  
  return(cuts)
  # Bayer and Diaconis model of splitting a deck of cards. Prob of a split at a card binom(n,k)*(1/2^n)
}

alternating.overhand.shuffle <- function(deck,prob=0.5)
#Shuffles a deck of cards in heaps, alternatingly putting a fraction of the deck above then below the new 'shuffled' deck
{
  
  cuts<-split.deck(deck,prob)
  cut.one<-cuts[[1]]
  cut.two<-cuts[[2]]
  
  shuffled.deck<-cut.one
  
  alternating = 1
  
  while(length(cut.one) > 1){
    
    cuts<-split.deck(cut.two,prob)
    cut.one<-cuts[[1]]
    
    if(alternating){
      shuffled.deck<-c(cut.one,shuffled.deck)
      alternating=0
    }else{
      shuffled.deck<-c(shuffled.deck,cut.one)
      alternating=1
    }
    
    cut.two<-cuts[[2]]
  }
  
   if(alternating){
      shuffled.deck<-c(cut.two,shuffled.deck)
    }else{
      shuffled.deck<-c(shuffled.deck,cut.two)
    }
  
  return(shuffled.deck)
  
}

consecutive.overhand.shuffle <- function(deck,prob=0.5)
#Shuffles a deck of cards in heaps, placing a fraction of one half (governed by prob) consecutively atop the other half
{
   
  cuts<-split.deck(deck,prob)
  cut.one<-cuts[[1]]
  cut.two<-cuts[[2]]
  
  shuffled.deck<-cut.one
      
  while(length(cut.one) != 0){
          
    cuts<-split.deck(cut.two,prob)
    cut.one<-cuts[[1]]
    
    shuffled.deck<-c(cut.one,shuffled.deck)

    cut.two<-cuts[[2]]
  }
  
  shuffled.deck<-c(cut.two,shuffled.deck)
  
  return(shuffled.deck)
}


riffle.shuffle <- function(deck)
#An implementation of the riffle shuffle model found in the Bayer Diaconis paper
{
  deck.size<-length(deck)
  cuts<-split.deck(deck,0.5) #Bayer Diaconis split
  
  shuffled.deck<-c()
    
  cut.one<-cuts[[1]]
  cut.two<-cuts[[2]]
  
  while(length(shuffled.deck) < deck.size){
    
    length.cut.one<-length(cut.one)
    length.cut.two<-length(cut.two)
      
    weights<-c(length.cut.one/deck.size,length.cut.two/deck.size) #Update weightings
    
    pile<-sample(c(1,2),1,prob=weights)#Choose which pile to take a card from
    
    if(pile == 1){
      
      shuffled.deck<-c(cut.one[length.cut.one],shuffled.deck)
      cut.one<-cut.one[-length.cut.one]
      
    }else{
      
      shuffled.deck<-c(cut.two[length.cut.two],shuffled.deck)
      cut.two<-cut.two[-length.cut.two]
    }
    
  }
  
  return(shuffled.deck)
}

cut.deck <- function(deck,prob=0.5)
{
  deck.size<-length(deck)
  cuts<-split.deck(deck,prob) #Bayer Diaconis split
     
  cut.one<-cuts[[1]]
  cut.two<-cuts[[2]]
  
  cut.deck<-c(cut.two,cut.one)
  
  return(cut.deck)
}

fischer.yates.shuffle <- function(deck)
{
  deck.size<-length(deck)
  shuffled.deck<-deck
  
  for(i in (deck.size):2){
    
    j<-sample(c(1:i),1)
    shuffled.deck<-swap.vector.elements(shuffled.deck,i,j)
  }
  
  return(shuffled.deck)
}

swap.vector.elements <- function(vector,i,j)
{
  element<-vector[j]
  vector[j]<-vector[i]
  vector[i]<-element
  
  return(vector)
}


pile.shuffle <- function(deck,Npiles=7)
{
  deck.size<-length(deck)
  piles<-vector("list",Npiles)
  
  for(i in 1:deck.size){
    
    index<-(i%%Npiles)+1
   
    current.pile<-piles[[index]]
    piles[[index]]<-c(deck[i], piles[[index]])
  }
  
  shuffled.deck<-c()
  
  for(pile in 1:Npiles){
    
    shuffled.deck<-c(piles[[pile]],shuffled.deck)
  }
  
  return(shuffled.deck)
}

beta.model.split.deck<-function(deck,alpha=2,beta=7)
#The values for alpha and beta were chosen as they fit a nice dstirbution as a prior for deck proportion
{
  
  split.prob<-rbeta(1,alpha,beta)
  
  cuts<-split.deck(deck,split.prob)
  
  return(cuts)
}

beta.alternating.overhand.shuffle <- function(deck)
#Shuffles a deck of cards in heaps, alternatingly putting a fraction of the deck above then below the new 'shuffled' deck
{
  
  cuts<-beta.model.split.deck(deck)
  cut.one<-cuts[[1]]
  cut.two<-cuts[[2]]
  
  shuffled.deck<-cut.one
  
  alternating = 1
  
  while(length(cut.one) != 0){
    
    cuts<-beta.model.split.deck(cut.two)
    cut.one<-cuts[[1]]
    
    if(alternating){
      shuffled.deck<-c(cut.one,shuffled.deck)
      alternating=0
    }else{
      shuffled.deck<-c(shuffled.deck,cut.one)
      alternating=1
    }
    
    cut.two<-cuts[[2]]
  }
  
   if(alternating){
      shuffled.deck<-c(cut.two,shuffled.deck)
    }else{
      shuffled.deck<-c(shuffled.deck,cut.two)
    }
  
  return(shuffled.deck)
  
}

beta.consecutive.overhand.shuffle <- function(deck)
#Shuffles a deck of cards in heaps, placing a fraction of one half (governed by prob) consecutively atop the other half
{
   
  cuts<-beta.model.split.deck(deck)
  cut.one<-cuts[[1]]
  cut.two<-cuts[[2]]
  
  shuffled.deck<-cut.one
      
  while(length(cut.one) != 0){
          
    cuts<-beta.model.split.deck(cut.two)
    cut.one<-cuts[[1]]
    
    shuffled.deck<-c(cut.one,shuffled.deck)

    cut.two<-cuts[[2]]
  }
  
  shuffled.deck<-c(cut.two,shuffled.deck)
  
  return(shuffled.deck)
}