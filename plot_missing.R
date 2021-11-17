plot_missing<-function(data,percent=FALSE){
  missing_patterns <- data.frame(is.na(data)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  if(percent){
    missing_patterns<-missing_patterns%>%
      mutate(count=count/sum(count)*100)
    }
  level_order<-names(sort(colSums(is.na(data)),decreasing=TRUE))
  data_m<-missing_patterns%>%subset(select=-count)
  id<-which(rowSums(data_m)==0)
  row<-nrow(data_m)
  col<-ncol(data_m)
  data_m<-data_m%>%mutate(ID = rownames(.))%>%
    melt(id.vars=c("ID"))%>%
    mutate(missing = ifelse(ID==id,2,ifelse(value=="TRUE", 1, 0)))
  
  mainplot<-ggplot(data_m, aes(x=factor(variable,levels=level_order), y=factor(ID,rev(unique(ID))))) +
    geom_tile(aes(fill=factor(missing)),color = "white") + 
    scale_fill_manual(values = alpha(c("grey", "blueviolet","grey"), c(.4,.4,0.9)))+
    geom_text(aes(x=(1+col)/2,y=row-id+1), label = "complete cases")+
    xlab('variable')+
    ylab('missing pattern')+
    guides(fill='none')+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
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
    theme_bw()+
    theme(panel.grid.major.x=element_blank())+
    xlab('')+
    ylab(ifelse(percent,'%rows missing','num rows missing'))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
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
