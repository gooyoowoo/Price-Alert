library(rvest);
library(tcltk);
library(tictoc);
library(beepr);
library(RSelenium);
library(svDialogs)
                                      #Amazon Price Alert (Need Firefox browser)
#Product's Amazon page
address<-dlgInput("Enter the product's Amazon web address: ")$res;

#Set price threshold
target<-as.numeric(dlgInput("Enter your price target: ")$res);

#How Many Hours To Run
Hours<-as.numeric(dlgInput("Enter how many hours to run program: ")$res);

check<-0;
hour<-0;
Min<-0;

#Interval to check (minutes)
Int<-as.numeric(dlgInput("Enter length of interval to check (minutes): ")$res);

while(check==0){
  
  tic();
  time<-as.numeric(format(as.POSIXct(Sys.time()),format="%M"));
  Rem<-time %% Int;
  
  if (Rem==0 & time!=Min){
    #Set webpath
    add<-read_html(address);
    price<-unique(add%>%html_nodes("[class=a-price-whole]")%>%html_text());
    price<-as.numeric(substr(price,1,nchar(price)));
    
    price2<-unique(add%>%html_nodes("[class=a-price-fraction]")%>%html_text());
    price2<-as.numeric(substr(price2,1,nchar(price2)));
    
    price<-price+price2/100;
    if (price<=target){
      #Alert sound
      beep(8);
      Name<-add%>%html_nodes("[id=productTitle]")%>%html_text;
      print(tkmessageBox(title = paste("Price Alert for", Name,sep=""),message = paste("Price is now below target!", " Current price is $", as.character(price),sep=" "), icon = "info", type = "ok"))
      
      rs_driver<-rsDriver(browser='firefox');
      remDr<-rs_driver$client;
      remDr$navigate(address);
      
      check<-1;
    }
   
    Min<-time;
  }
  
  h<-toc();
  hour<-hour+(h$toc-h$tic)/3600;
  sink()
   if(hour==Hours){
     check<-1;
   }
  Sys.sleep(10);
}