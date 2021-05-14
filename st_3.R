#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/Reports/ERCB/ST3")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/Reports/ERCB/ST3")
print(getwd())
source("../../../andrew_base.R")

require(readxl)


m3_bbl<-function(x) x*6.2898
bbl_m3<-function(x) x/6.2898

#grab the old data
keep_rows<-c("Crude Oil Light",  "Crude Oil Medium",
             "Crude Oil Heavy","Crude Oil Ultra Heavy","Total Conventional Oil Production",
             "Condensate Production","In Situ Production","Mined Production",
             "Non-Upgraded Total","Upgraded Production","Total Oil Sands Production",
             "Total Production")

data_store <- list()
years<-seq(2010,2016)
i<-1
for(year in years){
  #year<-2016
  filename<-paste("Oil_2010-2016.xlsx",sep="")
  production_data <- read_xlsx(filename, sheet = as.character(year), skip = 4)
  names(production_data)[1]<-"product"
  production_data<-production_data %>% select(-2,-15) %>% filter(product %in% keep_rows)
  names(production_data)[grep(as.character(year),names(production_data))]<-"annual"
  production_data$year<-year
  production_data<-production_data %>% melt(id=c("product","annual","year"),
                                            variable.name="month",value.name = "production")
  data_store[[i]]<-production_data
  i<-i+1
  }
  year<-2017
  filename<-paste("Oil_2017.xlsx",sep="")
  production_data <- read_xlsx(filename, sheet = "Data", skip = 4)
  names(production_data)[1]<-"product"
  production_data<-production_data %>% select(-2,-15) %>% filter(product %in% keep_rows)
  names(production_data)[grep(as.character(year),names(production_data))]<-"annual"
  production_data$year<-year
  production_data<-production_data %>% melt(id=c("product","annual","year"),
                                          variable.name="month",value.name = "production")
  data_store[[i]]<-production_data
  i<-i+1
  year<-2018
  filename<-paste("Oil_2018.xlsx",sep="")
  production_data <- read_xlsx(filename, sheet = "Data", skip = 4)
  names(production_data)[1]<-"product"
  production_data<-production_data %>% select(-2,-15) %>% filter(product %in% keep_rows)
  names(production_data)[grep(as.character(year),names(production_data))]<-"annual"
  production_data$year<-year
  production_data<-production_data %>% melt(id=c("product","annual","year"),
                                            variable.name="month",value.name = "production")
  
  
  data_store[[i]]<-production_data
  
  i<-i+1
  year<-2019
  filename<-paste("Oil_2019.xlsx",sep="")
  production_data <- read_xlsx(filename, sheet = "Data", skip = 4)
  names(production_data)[1]<-"product"
  production_data<-production_data %>% select(-2,-15) %>% filter(product %in% keep_rows)
  names(production_data)[grep(as.character(year),names(production_data))]<-"annual"
  production_data$year<-year
  production_data<-production_data %>% melt(id=c("product","annual","year"),
                                            variable.name="month",value.name = "production")
  
  data_store[[i]]<-production_data
  
  i<-i+1
  year<-2020
  filename<-paste("Oil_2020.xlsx",sep="")
  #download.file("https://www.aer.ca/documents/sts/st3/Oil_2020.xlsx",filename,mode="wb")
  production_data <- read_xlsx(filename, sheet = "Data", skip = 4)
  names(production_data)[1]<-"product"
  production_data<-production_data %>% select(-2,-15) %>% filter(product %in% keep_rows)
  names(production_data)[grep(as.character(year),names(production_data))]<-"annual"
  production_data$year<-year
  production_data<-production_data %>% melt(id=c("product","annual","year"),
                                            variable.name="month",value.name = "production")
  
  data_store[[i]]<-production_data
  
  
  i<-i+1
  year<-"Current"
  filename<-paste("Oil_current.xlsx",sep="")
  #check for internet
  if(has_internet()){
    download.file("https://www.aer.ca/documents/sts/st3/Oil_current.xlsx",filename,mode="wb")
  }
  
  production_data <- read_xlsx(filename, sheet = "Data", skip = 4)
  year<-2021
  names(production_data)[1]<-"product"
  production_data<-production_data %>% select(-2,-15) %>% filter(product %in% keep_rows)
  names(production_data)[grep(as.character(year),names(production_data))]<-"annual"
  production_data$year<-year
  production_data<-production_data %>% melt(id=c("product","annual","year"),
                                            variable.name="month",value.name = "production")
  production_data<-production_data %>% filter(production!=0)
  data_store[[i]]<-production_data
  
  all_production<-do.call(rbind,data_store)
  all_production<-all_production %>% mutate(production=as.numeric(production),
                                            date=ymd(paste(year,as.character(month),1,sep="-")))
         

  graph_data<-c("Conventional Light Oil Production","Conventional Heavy Oil Production","Condensate Production",
               "In Situ Bitumen Production","Mined Bitumen Production")
  
  all_production <-all_production %>% mutate(product=as_factor(product)) %>%mutate(
                                             product=fct_recode(product,
                                                                "In Situ Bitumen Production"="In Situ Production",
                                                                "Mined Bitumen Production"="Mined Production"
                                                                ),
                                             product=fct_relevel(product,"Mined Bitumen Production",after=5))

  
  
   
  all_production <-all_production %>% mutate(product=fct_collapse(product,
                        "Conventional Light Oil Production" = c("Crude Oil Light","Crude Oil Medium"),
                        "Conventional Heavy Oil Production"= c("Crude Oil Heavy","Crude Oil Ultra Heavy")
                        )) %>% 
                    group_by(product,year,month,date)%>%
                          summarize(production=sum(production),annual=sum(as.numeric(annual))) %>%
                          ungroup()
    
  
  all_production <-all_production %>% mutate(year=year(date), quarter=quarter(date))%>%
    group_by(product)%>% arrange(date)%>% mutate(
      roll_4m=roll_mean(production,4),
      roll_12m=roll_sum(production,12),
      growth_12m=(roll_12m-lag(roll_12m,12))/lag(roll_12m,12),
      lag12_raw=(production-lag(production,12))/lag(production,12),
      lag12_roll=(roll_4m-lag(roll_4m,12))/lag(roll_4m,12))
  
  
  annual<-all_production %>% group_by(year,product) %>% summarise(production=sum(production))
  annual <-annual %>%arrange(year)%>% group_by(product)%>%
    mutate(yoy_growth=round((production-lag(production,1))/lag(production,1)*100,2))
  
  
  
  quarterly<-all_production %>% group_by(year,quarter,product) %>% summarise(production=mean(production))
  quarterly <-quarterly %>%arrange(year,quarter)%>% group_by(product)%>%
    mutate(q_lasty=round((production-lag(production,4))/lag(production,4)*100,2))
  
ggplot(filter(all_production,product%in%graph_data))+
  geom_area(aes(date,m3_bbl(production)/days_in_month(date)/10^6,group=product,fill=product))+
  scale_fill_manual("",values = colors_tableau10(),guide = "legend")+
  scale_x_date(date_breaks = "6 months",date_labels =  "%b\n%Y",expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  #scale_colour_manual("",values=my_palette,guide = "legend")+
  guides(fill=guide_legend(nrow=2))+
  expand_limits(x = ymd("2020-02-01"))+
  theme(panel.border = element_blank(),
        plot.margin=margin(t = 0, r = 15, b = 0, l = 0, unit = "pt"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        axis.text = element_text(size = 12,),
        axis.text.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title = element_text(size = 12),
        #axis.label.x = element_text(size=20,vjust=+5),
        plot.subtitle = element_text(size = 12,hjust=0.5),
        plot.caption = element_text(face="italic",size = 12,hjust=0),
        legend.key.width=unit(2,"line"),
        legend.position = "bottom",
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Oil and Bitumen Production (Millions of barrels per day)",x=NULL,
       title="Alberta Conventional Oil and Bitumen Production",
       #subtitle="For Operators with Production above 25k bbl/d",
       caption=paste("Source: AER ST-3 data current to ",format.Date(max(all_production$date),format = "%B, %Y")  ,", graph by @andrew_leach",sep=""))
  ggsave("oil_prod.png",width=12,height=6)

ggplot(filter(all_production,product%in%graph_data,product!="Total Conventional Oil Production",product!="Conventional Heavy Oil Production",product!="Conventional Light Oil Production",product!="Condensate Production"))+
  geom_area(aes(date,m3_bbl(production)/days_in_month(date)/10^6,group=product,fill=product))+
  scale_fill_manual("",values = colors_tableau10(),guide = "legend")+
  scale_x_date(date_breaks = "6 months",date_labels =  "%b\n%Y",,expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  #scale_colour_manual("",values=my_palette,guide = "legend")+
  #guides(fill=FALSE,colour=FALSE)+
  theme_classic() +
  guides(fill=guide_legend(nrow=1))+
  theme(plot.margin=margin(t = 0, r = 15, b = 0, l = 0, unit = "pt"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        axis.text = element_text(size = 12,),
        axis.text.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title = element_text(size = 12),
        #axis.label.x = element_text(size=20,vjust=+5),
        plot.subtitle = element_text(size = 12,hjust=0.5),
        plot.caption = element_text(face="italic",size = 12,hjust=0),
        legend.key.width=unit(2,"line"),
        legend.position = "bottom",
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Oil Sands Bitumen Production (millions of barrels per day)",x=NULL,
       #title="Alberta Oil Sands Bitumen Production",
       #subtitle="For Operators with Production above 25k bbl/d",
       #caption=paste("Source: AER ST-3 data current to ",format.Date(max(all_production$date),format = "%B %Y")  ,", graph by @andrew_leach",sep=""),
       NULL)
ggsave("oil_sands_prod.png",width=12,height=6)

#convert to emissions inventory subsectors

#"Conventional Oil Production"
#"Oil Sands Mining and Extraction"
#"In-situ Bitumen"
#"Upgrading"



ghg_production <-filter(all_production,product %in% c("In Situ Bitumen Production","Mined Bitumen Production",
                                                      "Upgraded Production","Conventional Light Oil Production","Conventional Heavy Oil Production"))%>%
  mutate(product=fct_drop(product),product=fct_recode(product,
                                              "In-situ Bitumen"="In Situ Bitumen Production",
                                              "Oil Sands Mining and Extraction"="Mined Bitumen Production",
                                              "Upgrading"="Upgraded Production")) %>%
  group_by(year,product) %>% summarize(production=sum(m3_bbl(production)/10^6))

save(ghg_production,file = "../../../st3_link.Rdata") #save to link to NIR data

#save(ghg_production,file = "st3_link.Rdata") #save to link to NIR data




