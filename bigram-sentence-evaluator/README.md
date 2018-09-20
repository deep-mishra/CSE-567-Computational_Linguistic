## Bigram Model Sentence Evaluator
**Problem:** Prolog project to evaluate the correctness of english sentence using bigram model

**Approach:**
The project constructs a Prolog bigram language model using small [DA_Corpus.text](DA_Corpus.txt) corpus.

Steps taken ([bigram_model.pl](bigram_model.pl)):

1. The [DA_Corpus.text](DA_Corpus.txt) corpus is normalized using [unix](unix_commands.txt) commands.
2. Created a prolog readable [unigram.pl](bigram-sentence-evaluator/unigrams.pl) and [bigram.pl](bigram-sentence-evaluator/bigrams.pl) database from normalized corpus.
3. In the final step, implemented [bigram_model.pl](bigram-sentence-evaluator/bigram_model.pl) which computes the probability of any word sequence, of any size, via a predicate called **calc_prob/2**. The predicate calc_prob/2 works in log space and applies laplace smoothing on fly to compute the probability of given sentence.

**Sample outputs:** 
As shown in the output below, sentence like "the book fell" will have better value than "i fell on the book"

![output1](bigram-sentence-evaluator/output/output1.png)

Similarly the sentence like "the book that he wanted fell on my feet" will have better value than "book the that he wanted fell on my feet"

![output2](bigram-sentence-evaluator/output/output2.png)

