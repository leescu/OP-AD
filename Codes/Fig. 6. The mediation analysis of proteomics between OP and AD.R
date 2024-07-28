# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# +++++++section 1 A+++++++++ ####
library(AnnotationDbi)
library(org.Hs.eg.db)
library(clusterProfiler)
library(dplyr)
library(ggplot2)
load(file="original data/mediation_results.Rdata")
mediation_result<-mediation_results
mediation_result$Mediator<-toupper(mediation_result$Mediator)

mediation_result$ACME<-as.numeric(mediation_result$ACME)
write.csv(mediation_result,file = "gene_result/mediation_result.csv",row.names = T)
mediation<-subset(mediation_result,ACME_p_value==0&Prop_Mediated_p_value<0.05)

gene_result<-subset(mediation_result,ACME_p_value<0.050)

gene.df <- bitr(gene_result$Mediator,fromType="SYMBOL",toType="ENTREZID", OrgDb = org.Hs.eg.db)
gene <- gene.df$ENTREZID
gene.df$logFC <- gene_result$ACME
gene.df1<-gene.df[order(-gene.df$logFC),]

genelist = gene.df1$logFC
names(genelist) = gene.df1$ENTREZID 
head(genelist)

mediation_results$ACME_p_value[mediation_results$ACME_p_value == 0] <- 1e-300

mediation_results$logP <- -log10(mediation_results$ACME_p_value)


Q1_ACME <- quantile(mediation_results$ACME, 0.25, na.rm = TRUE)
Q3_ACME <- quantile(mediation_results$ACME, 0.75, na.rm = TRUE)
IQR_ACME <- Q3_ACME - Q1_ACME

Q1_logP <- quantile(mediation_results$logP, 0.25, na.rm = TRUE)
Q3_logP <- quantile(mediation_results$logP, 0.75, na.rm = TRUE)
IQR_logP <- Q3_logP - Q1_logP


lower_bound_ACME <- Q1_ACME - 1.5 * IQR_ACME
upper_bound_ACME <- Q3_ACME + 1.5 * IQR_ACME

lower_bound_logP <- Q1_logP - 1.5 * IQR_logP
upper_bound_logP <- Q3_logP + 1.5 * IQR_logP


filtered_results <- mediation_results[
  mediation_results$ACME >= lower_bound_ACME & mediation_results$ACME <= upper_bound_ACME &
    mediation_results$logP >= lower_bound_logP & mediation_results$logP <= upper_bound_logP, 
]


ACME_threshold <- 1
p_value_threshold <- -log10(0.05)


ggplot(filtered_results, aes(x = ACME, y = logP)) +
  geom_point(aes(color = (logP > p_value_threshold) & (abs(ACME) > ACME_threshold)), size = 3.5) +
  scale_color_manual(values = c("grey", "#BD4146")) +
  theme_minimal() +
  labs(title = "Volcano Plot",
       x = "ACME",
       y = "-log10(P-value)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(size = 1.2, color = "black")
  ) +
  geom_hline(yintercept = p_value_threshold, linetype = "dashed", color = "#86D3DE", size = 1.2) +
  geom_vline(xintercept = c(-ACME_threshold, ACME_threshold), linetype = "dashed", color = "#86D3DE", size = 1.2)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# +++++++section 2 B,C+++++++++ ####
library(msigdbr)

m_t2g <- msigdbr(species = "Homo sapiens", category = "H") %>% 
  dplyr::select(gs_name, entrez_gene)
head(m_t2g)

gsea_res <- GSEA(genelist, 
                 TERM2GENE = m_t2g,
                 minGSSize = 10,
                 maxGSSize = 500,
                 pvalueCutoff = 1,
                 pAdjustMethod = "BH"
)

gsea_res[[gsea_res$ID[[1]]]]


Go_gseresult <- gseGO(genelist, 'org.Hs.eg.db', keyType = "ENTREZID", ont="BP", nPerm = 1000, minGSSize = 10, maxGSSize = 1000, pvalueCutoff=0.05)

ego_ALL <- enrichGO(gene = gene,
                   OrgDb=org.Hs.eg.db,
                   keyType = "ENTREZID",
                   ont = "ALL",
                   pAdjustMethod = "BH",
                   minGSSize = 1,
                   pvalueCutoff = 0.01,
                   qvalueCutoff = 0.05,
                   readable = TRUE)
                   
ego_CC <- enrichGO(gene = gene,
                   OrgDb=org.Hs.eg.db,
                   keyType = "ENTREZID",
                   ont = "CC",
                   pAdjustMethod = "BH",
                   minGSSize = 1,
                   pvalueCutoff = 0.01,
                   qvalueCutoff = 0.05,
                   readable = TRUE)

ego_BP <- enrichGO(gene = gene,
                   OrgDb=org.Hs.eg.db,
                   keyType = "ENTREZID",
                   ont = "BP",
                   pAdjustMethod = "BH",
                   minGSSize = 1,
                   pvalueCutoff = 0.01,
                   qvalueCutoff = 0.05,
                   readable = TRUE)

ego_MF <- enrichGO(gene = gene,
                   OrgDb=org.Hs.eg.db,
                   keyType = "ENTREZID",
                   ont = "MF",
                   pAdjustMethod = "BH",
                   minGSSize = 1,
                   pvalueCutoff = 0.01,
                   qvalueCutoff = 0.05,
                   readable = TRUE)


ego_ALL <- as.data.frame(ego_ALL)
ego_result_BP <- as.data.frame(ego_BP)
ego_result_CC <- as.data.frame(ego_CC)
ego_result_MF <- as.data.frame(ego_MF)
ego <- rbind(ego_result_BP,ego_result_CC,ego_result_MF)
write.csv(ego_ALL,file = "gene_result/ego_ALL.csv",row.names = T)
write.csv(ego_result_BP,file = "gene_result/ego_result_BP.csv",row.names = T)
write.csv(ego_result_CC,file = "gene_result/ego_result_CC.csv",row.names = T)
write.csv(ego_result_MF,file = "gene_result/ego_result_MF.csv",row.names = T)
write.csv(ego,file = "gene_result/ego.csv",row.names = T)

display_number = c(22, 22, 22)
ego_result_BP <- as.data.frame(ego_BP)[1:display_number[1], ]
ego_result_CC <- as.data.frame(ego_CC)[1:display_number[2], ]
ego_result_MF <- as.data.frame(ego_MF)[1:display_number[3], ]


go_enrich_df <- data.frame(
ID=c(ego_result_BP$ID, ego_result_CC$ID, ego_result_MF$ID),                         Description=c(ego_result_BP$Description,ego_result_CC$Description,ego_result_MF$Description),
GeneNumber=c(ego_result_BP$Count, ego_result_CC$Count, ego_result_MF$Count),
type=factor(c(rep("biological process", display_number[1]), 
              rep("cellular component", display_number[2]),
              rep("molecular function", display_number[3])), 
              levels=c("biological process", "cellular component","molecular function" )))


for(i in 1:nrow(go_enrich_df)){
  description_splite=strsplit(go_enrich_df$Description[i],split = " ")
  description_collapse=paste(description_splite[[1]][1:5],collapse = " ") 
  go_enrich_df$Description[i]=description_collapse
  go_enrich_df$Description=gsub(pattern = "NA","",go_enrich_df$Description)
}


go_enrich_df$type_order=factor(rev(as.integer(rownames(go_enrich_df))),labels=rev(go_enrich_df$Description))

ggplot(data=go_enrich_df, aes(x=type_order,y=GeneNumber, fill=type)) +
  geom_bar(stat="identity", width=0.8) + 
  scale_fill_manual(values = COLS) + 
  coord_flip() + 
  xlab("GO term") + 
  ylab("Gene number") + 
  labs(title = "The Most Enriched GO Terms")+
  theme_bw()
 

go_enrich_df$type_order=factor(rev(as.integer(rownames(go_enrich_df))),labels=rev(go_enrich_df$Description))
COLS <- c("#66C3A5", "#8DA1CB", "#FD8D62")
 ggplot(data=go_enrich_df, aes(x=type_order,y=GeneNumber, fill=type)) + 
  geom_bar(stat="identity", width=0.8) + 
  scale_fill_manual(values = COLS) + 
  theme_bw() + 
  xlab("GO term") + 
  ylab("Num of Genes") + 
  labs(title = "The Most Enriched GO Terms")+ 
  theme(axis.text.x=element_text(face = "bold", color="gray50",angle = 70,vjust = 1, hjust = 1 ))

 
 

 kk <- enrichKEGG(gene = gene,keyType = "kegg",organism= "human", qvalueCutoff = 0.05, pvalueCutoff=0.05)
 

 hh <- as.data.frame(kk)
 rownames(hh) <- 1:nrow(hh)
 hh$order=factor(rev(as.integer(rownames(hh))),labels = rev(hh$Description))
 ggplot(hh,aes(y=order,x=Count,fill=p.adjust))+
   geom_bar(stat = "identity",width=0.7)+
   scale_fill_gradient(low = "red",high ="blue" )+
   labs(title = "KEGG Pathways Enrichment",
        x = "Gene numbers", 
        y = "Pathways")+
   theme(axis.title.x = element_text(face = "bold",size = 16),
         axis.title.y = element_text(face = "bold",size = 16),
         legend.title = element_text(face = "bold",size = 16))+
   theme_bw()
 hh <- as.data.frame(kk)
 rownames(hh) <- 1:nrow(hh)
 hh$order=factor(rev(as.integer(rownames(hh))),labels = rev(hh$Description))
 ggplot(hh,aes(y=order,x=Count))+
   geom_point(aes(size=Count,color=-1*p.adjust))+
   scale_color_gradient(low="green",high = "red")+
   labs(color=expression(p.adjust,size="Count"), 
        x="Gene Number",y="Pathways",title="KEGG Pathway Enrichment")+
   theme_bw()
