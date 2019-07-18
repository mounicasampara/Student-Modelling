# Student-Modelling
This is a website that runs through Shniy. It is an adaptive e-learning platform which is used for preparation for tests like GRE. Instead of a one-size-fits-all model of other platforms, this website gives you tailored practice and analysis. It gives adaptive training based on user's performance and according to requirement. 
It determines the knowledge level of the user in all the topics related to the examination. I have designed an algorithm that is derived from Dempster-Shafer theory. By taking the statistics of answers, i determine the probability that the user would answer a question based on that concept correctly.

Working: We take 2 questions(say Q1 and Q2), with pre-defined proportions of each topic that the question is comprised of.

There are 3 knowledge levels I considered to design the algorithm which are determined from the pretest taken by the user which helps as the base case for all the test cases. Each choice they make helps to detect user's understanding and classifies user's knowledge level into either of the 3 : L(Low), I(Intermediate), M(Master). When the user answers the questions, depending on whether they were right or wrong, the algorithm assigns Q1 and Q2 with M or L respectively.Therefore, the frame of discernment is {null, {L},{I},{M},{LI},{IM},{LM},{LIM}}, namely low level, intermediate level, master level, beginning level({LI}), advanced level({IM}).

For example, user must know C1(concept1) and C2(concept2) to answer Q1 correctly. Q2 only needs C2. The total weight of all the relationships from question to concepts must be 1 which is used as the mass function in the DS theory. These weights are defined by the expert in the domain. For instance, let the weight of Q1 to C1 is 0.6 and 0.4 belongs to C2. 

If the user answered Q1 correctly but not Q2, M(mastered the concept in Q1) is allotted with 0.6 for Q1 and L(Low knowledge level of topic in Q2 and therefore answered incorrectly) is allotted with 0.5. The remaining 0.4 and 0.5 is distributed among other knowledge levels using these derived formulae:
            if remainder probability is even: X->v1/2-1 and Y -> v1/2+1 where X and Y can either be LI or LM K-level depending on the correctness of the answer.
            if remainder probability is odd: X->(v1-1)/2 and Y -> (v1+1)/2 where X and Y can either be LI or LM K-level depending on the correctness of the answer.
            

To determine the knowledge level of the user in the topic or the probability that the user will correctly answer a question that is related to this topic, we m1 xor m2. This method is the COMBINATION RULE OF EVIDENCE.
            

