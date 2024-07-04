from rouge_score import rouge_scorer
scorer = rouge_scorer.RougeScorer(['rouge1'], use_stemmer=True)    
#gold is the online sumamries in succession
#a is the lexRankr summary
#b is the rwa summary

#lexRankr
a = “Although smoking has been decreasing over the past decade, residents in rural Appalachian counties of Virginia exhibit alarmingly high rates of cigarette smoking, which likely will result in remarkable and yet preventable health and economic consequences.  We analyzed the impact of SVI level and the combination of rurality with Appalachian status on county-level cigarette smoking prevalence, controlling for factors such as coal mining, tobacco agriculture, and health care provider shortages using multivariate regression analysis with robust SEs. We mapped SVI with estimated current smoking prevalence along with rural identifiers on Virginia counties using Census shape data files. Despite rural Appalachian counties having the highest cigarette smoking rates in Virginia, they did not have the highest SVI levels.”

#rwa
b = “Although the prevalence of current cigarette smoking among US adults has decreased over the past several decades to 13.7% in 2018, this decrease has not been as pronounced in rural areas, such as rural Appalachia (as high as 33% in some counties). To obtain reliable estimates for counties with missing or small sample sizes, we followed standard procedure and combined BRFSS data from 2011 to 2019 into 3 periods of 3 years each (2011 to 2013, 2014 to 2016, 2017 to 2019). Because these factors make up 2 dimensions of the SVI, the association of SVI score with smoking prevalence by rurality and Appalachian status in our study warrants further investigation to identify the role each dimension of the SVI plays in the elevated smoking prevalence in rural Appalachia.”
#for paraphraser
gold = “Although the prevalence of current cigarette smoking among US adults has decreased over the past several decades to 13. The Appalachian region, which extends across 13 states, has historically been characterized by its mountainous terrain, poverty, limited health care access, and reliance on tobacco agriculture and coal mining, which may contribute to elevated smoking rates. These components are closely tied to a population`s health care access and adherence to health guidelines, which may affect the effectiveness of tobacco control initiatives in rural areas with scarce resources or areas with a high SVI.”                                             

scores = scorer.score(b, gold)

scores

scores = scorer.score(a, gold)

scores

# for ahrefs
gold = "Cigarette smoking is the leading cause of preventable morbidity and mortality in the United States. Smoking rates have decreased in the US overall, but not as much in rural areas like rural Appalachia. Factors such as poverty, limited healthcare access, and reliance on tobacco agriculture and coal mining contribute to higher smoking rates in rural Appalachia."                           

scores = scorer.score(b, gold)

scores

scores = scorer.score(a, gold)

scores

