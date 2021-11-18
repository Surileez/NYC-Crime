plot_missing<-function(data,percent=FALSE,angle=0,vjust=0.5,hjust=0.5){
  missing_patterns <- data.frame(is.na(data)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  if(percent){
    missing_patterns<-missing_patterns%>%
      mutate(count=count/sum(count)*100)
  }
  level_order<-names(sort(colSums(is.na(data)),decreasing= TRUE))
  data_m<-missing_patterns%>%subset(select=-count)
  id<-which(rowSums(data_m)==0)
  if (length(id)==0){
    id=-1
  }
  row<-nrow(data_m)
  col<-ncol(data_m)
  data_m<-data_m%>%mutate(ID = rownames(.))%>%
    melt(id.vars=c("ID"))%>%
    mutate(missing = ifelse(ID==id,-1,ifelse(value == "TRUE", 0, 1)))
  
  mainplot<-ggplot(data_m, aes(x=factor(variable,levels=level_order), y=factor(ID,rev(unique(ID))))) +
    geom_tile(aes(fill=factor(missing)),color = "white") + 
    xlab('variable')+
    ylab('missing pattern')+
    guides(fill='none')+
    theme_classic()+
    theme(axis.text.x = element_text(angle = angle, vjust=vjust, hjust=hjust))
  if(id!=-1){
    mainplot<-mainplot+
      scale_fill_manual(values = alpha(c("grey","blueviolet","grey"), c(.9,.4,.4)))+
      geom_text(aes(x=(1+col)/2,y=row-id+1), label = "complete cases")
  }
  else{
    mainplot<-mainplot+scale_fill_manual(values = alpha(c("blueviolet","grey"), c(.4,.4)))
  }
  
  rowcount<-ggplot(missing_patterns,aes(x=factor(rownames(missing_patterns),rev(rownames(missing_patterns))),y=count))+
    geom_bar(stat='identity',fill=ifelse(rownames(missing_patterns)==id,'cornflowerblue',alpha('cornflowerblue',0.5)))+
    xlab('')+
    ylab(ifelse(percent,'%rows','row count'))+
    theme_bw()+
    theme(panel.grid.major.y=element_blank())+
    coord_flip()
  if(percent){
    rowcount<-rowcount+ylim(0,100)
  }
  
  df<-data.frame(colSums(is.na(data)))
  colnames(df)<-'count'
  if(percent){
    df<-df%>%mutate(count=count/nrow(data)*100)
  }
  numrows<-ggplot(df,aes(x=factor(rownames(df),levels=level_order),y=count))+
    geom_bar(stat='identity',fill=alpha('cornflowerblue',0.5))+
    ggtitle('missing value patterns')+
    xlab('')+
    ylab(ifelse(percent,'%rows missing','num rows missing'))+
    theme_bw()+
    theme(panel.grid.major.x=element_blank())+
    theme(axis.text.x = element_text(angle = angle, vjust=vjust, hjust=hjust))
  
  if(percent){
    numrows<-numrows+ylim(0,100)
  }
  
  design <- "
111#
2223
2223
2223
"
  numrows+mainplot+rowcount+plot_layout(design=design)
}

