from rouge_score import rouge_scorer
scorer = rouge_scorer.RougeScorer(['rouge1'], use_stemmer=True)    
#gold is the online sumamries in succession
#a is the lexRankr summary
#b is the rwa summary
scores = scorer.score(b, gold)
