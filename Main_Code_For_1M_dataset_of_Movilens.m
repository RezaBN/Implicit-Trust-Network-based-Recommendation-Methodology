clc;
clear;
close all;
tic,
%% load Data , Init

 %% Movielens
  load('Data1.mat');
  RatingData = Data1;  

   NumberUser = 6040;  
   NumberItem = 3900;

%% Making user-item matrix wich represents the rating of users to items
[NumberRates,~]= size(RatingData);

DataMatrix=zeros(NumberUser,NumberItem);

for i=1:NumberRates
    
     UserId = RatingData(i,1);  
     ItemId = RatingData(i,2);    
     Ranking = RatingData(i,3);   
     
     DataMatrix(UserId,ItemId)=Ranking;
  
end


  %%% compute average rating for users
  Average=zeros(NumberUser,1);
for q=1:NumberUser
    
     count1=0;
     AVrate1=0;
     
  for p=1:NumberItem
      
        if DataMatrix(q,p)>0
            
             AVrate1=AVrate1+DataMatrix(q,p);
             count1=count1+1;
           
        end
  end
    
     Average(q,1)=AVrate1/count1;
     
end

    DataMatrix=[DataMatrix Average];
    

%% Similarity

PCC = corrcoef((DataMatrix(:,1:NumberItem))');

%%% Computing the distance
 Dist_ij=zeros(NumberUser,NumberUser);
 Dist=[];
for i=1:NumberUser
    
    for j=1:NumberUser
        DR=0;
        if i~=j
            
            for k=1:NumberItem
                
                if DataMatrix(i,k)~=0 && DataMatrix(j,k)~=0
                    
                    DR=DR+((DataMatrix(i,k)-DataMatrix(j,k))^2);
                    
                end
            end
        end
        
        Dist_ij(i,j)=sqrt(DR);
    end
    Dist=[Dist;(Dist_ij(i,:)./(sum(Dist_ij(i,:))))];
    
end
%%% #%&$
Dist = 1 - ( Dist./1 );


Similarity= zeros(NumberUser,NumberUser);
for i=1:NumberUser
     for j=1:NumberUser
         if PCC(i,j) > 0
             Similarity(i,j) = (2 * ( ( PCC(i,j) * Dist(i,j) ) / (PCC(i,j) + Dist(i,j)) ) );
         end
     end
end

% Treshould of similarity
Q = ((sum((Similarity')))-1)/(NumberUser-1);

%% Confidence

Confidence=zeros(NumberUser,NumberUser);

for i=1:NumberUser
    
    SimilarCoun_i = (sum(Similarity(i,:)>=Q(i)))-1;
    ItemCoun_i = sum(DataMatrix(i,:)~=0);
    
    if SimilarCoun_i==0
        SimilarCoun_i=1;
    end
    if ItemCoun_i==0
        ItemCoun_i=1;
    end
    
    for j=1:NumberUser
        if i~=j
           CoSimiCount=0;
           CoItemCount=0;
        
           for u=1:NumberUser
               if i~=u && j~=u
                  if Similarity(i,u) >= Q(i) && Similarity(j,u) >= Q(j) 
                
                     CoSimiCount = CoSimiCount+1;

                  end
               end
           end
           CommonSimilars = CoSimiCount/SimilarCoun_i;
        
           for k=1:NumberItem
            
               if DataMatrix(i,k)~=0 && DataMatrix(j,k)~=0
                
                  CoItemCount=CoItemCount+1;
               
               end
            
           end
           CommonItems = CoItemCount/ItemCoun_i;
        
           Confidence(i,j)=(CommonSimilars+CommonItems)/2;
        end
    end
end

C = ((sum((Confidence')))-1)/(NumberUser-1); % Threshould of Confidence

 
%% Identical Opinion

E=0.1; % epsilon treshould

SameOpinion=zeros(NumberUser,NumberUser);

for i=1:NumberUser
    
    for j=1:NumberUser
        if i~=j
            
           CoSimiCount=0;
           SameSimiCount=0;
           SameConfCount=0;
           CoItemCount=0;
           SameRateCount=0;
           
           for u=1:NumberUser
               if i~=u && j~=u
                   
                  if Similarity(i,u) >= Q(i) && Similarity(j,u) >= Q(j)
                      
                     if (Similarity(i,u) - Similarity(j,u)) >= E 
                
                        SameSimiCount = SameSimiCount+1;

                     end 
                
                     CoSimiCount = CoSimiCount+1;

                  end
                  if Confidence(i,u) >= C(i) && Confidence(j,u) >= C(j)    
                      if (Confidence(i,u) - Confidence(j,u)) >= E
                      
                           SameConfCount = SameConfCount+1;
                      end
                  end 
                                 
               end
           end
           
           if CoSimiCount==0
              CoSimiCount=1;
           end
           SameSimiValue = SameSimiCount/CoSimiCount;
           SameConfValue = SameConfCount/(NumberItem-2);
           
           for k=1:NumberItem
            
               if DataMatrix(i,k)~=0 && DataMatrix(j,k)~=0
                   
                  if DataMatrix(i,k) == DataMatrix(j,k)
                      
                     SameRateCount=SameRateCount+1;
                     
                  end
                  
                  CoItemCount=CoItemCount+1;
                  
               end
            
           end
           
           if CoItemCount==0
              CoItemCount=1; 
           end
           SameRatingValue = SameRateCount/CoItemCount;
        
           SameOpinion(i,j)=(SameSimiValue+SameConfValue+SameRatingValue)/3;
        end
    end
end

O = (sum((SameOpinion')))/NumberUser;

%% Trust Network constracting
   
% Direct Trust

DirectTrust=zeros(NumberUser,NumberUser);

for i=1:NumberUser
    
    for j=1:NumberUser
        
        if Similarity(i,j) >= Q(i) && Confidence(i,j) >= C(i) && SameOpinion(i,j) >= O(i)       
           
            DirectTrust(i,j) = ( 3 * ( Similarity(i,j) * Confidence(i,j) * SameOpinion(i,j) ) ) / ( Similarity(i,j) + Confidence(i,j) + SameOpinion(i,j) ) ;
        
        elseif Similarity(i,j) >= Q(i) && Confidence(i,j) >= C(i) && SameOpinion(i,j) < O(i) 
            
            DirectTrust(i,j)= ( 2 * ( Similarity(i,j) * Confidence(i,j) ) ) / ( Similarity(i,j) + Confidence(i,j) ) ;

        elseif Similarity(i,j) >= Q(i) && Confidence(i,j) < C(i) && SameOpinion(i,j) >= O(i) 
            
            DirectTrust(i,j)= ( 2 * ( Similarity(i,j) * SameOpinion(i,j) ) ) / ( Similarity(i,j) + SameOpinion(i,j) ) ;  
            
        elseif Similarity(i,j) < Q(i) && Confidence(i,j) >= C(i) && SameOpinion(i,j) >= O(i) 
            
            DirectTrust(i,j)= ( 2 * ( Confidence(i,j) * SameOpinion(i,j) ) ) / ( Confidence(i,j) + SameOpinion(i,j) ) ; 
            
        elseif Similarity(i,j) >= Q(i) && Confidence(i,j) < C(i) && SameOpinion(i,j) < O(i) 
        
        else
            DirectTrust(i,j)= 0;
        end
        
    end
    
end

DirectTrust = sqrt (DirectTrust);

B = (sum((DirectTrust')))/NumberUser; % Treshould

% Propagating the direct trust

PropagatedTrust = zeros(NumberUser,NumberUser);

for i=1:NumberUser
    
    for j=1:NumberUser
        
        CoTrustCont=0;
        Mul=0;
        if i~=j
            
           for u=1:NumberUser
               
               if i~=u && j~=u
                   
                  if DirectTrust(i,u) >= B(i) && DirectTrust(u,j) >= B(u)
                      
                     Mul = Mul + ( 2 * ( ( DirectTrust(i,u) * DirectTrust(u,j) ) / (DirectTrust(i,u) + DirectTrust(u,j) ) ) );
                     CoTrustCont=CoTrustCont+1;
                     
                  end
               end
           end
           
           if CoTrustCont==0
               CoTrustCont=1;
           end
           PropagatedTrust(i,j) = Mul / CoTrustCont ;
           
        end
    end
end


%% Incipient Trust

 InTrust = zeros(NumberUser,NumberUser); 
for i=1:NumberUser
    for j=1:NumberUser
        
        if DirectTrust(i,j)>0 && PropagatedTrust(i,j)>0
           
            InTrust(i,j) = sqrt ((DirectTrust(i,j)*PropagatedTrust(i,j))/(DirectTrust(i,j)+PropagatedTrust(i,j)));
            
        elseif DirectTrust(i,j)==0 && PropagatedTrust(i,j)>0
            
            InTrust(i,j) = PropagatedTrust(i,j);
            
        elseif DirectTrust(i,j)>0 && PropagatedTrust(i,j)==0
            
            InTrust(i,j) = DirectTrust(i,j);
            
        else
            InTrust(i,j) = 0;
        end
        
    end
end
        
 
In = (sum((InTrust')))/NumberUser; % Trust Threshould

InITN = InTrust >= In; % Initial Trust Network

%% Incipient Trust Measuring

Rating=zeros(NumberUser,NumberItem);
Prec=zeros(NumberUser,NumberUser);

MaxRating=5;
MinRating=1;

%%% in FilmTrust
% MaxRating=4;
% MinRating=0.5;

for i=1:NumberUser
    
    for j=1:NumberUser
        
        ErorrSum=0;
        ItemCont=0;
        
        if i~=j && InTrust(i,j)>=In(i)
           
           for k=1:NumberItem
               
               Rating(i,k) = DataMatrix(i,NumberItem+1) + ( InTrust(i,j) * (DataMatrix(j,k) - DataMatrix(j,NumberItem+1) ) ) ;
               
               if DataMatrix(i,k)~=0
                  
                  Erorr = ( abs ( Rating(i,k) - DataMatrix(i,k) ) ) ;
                  Fraction = max((abs(MaxRating-DataMatrix(i,k))),(abs(MinRating-DataMatrix(i,k))));
                  ErorrSum = ErorrSum + (Erorr/Fraction);
                  ItemCont=ItemCont+1;
                  
               end
           end
           
           if ItemCont==0
              ItemCont=1; 
           end
           Prec(i,j) = ErorrSum/ItemCont;
           
        end
        if i==j
           Prec(i,j) = 1;
        end
    end
end

D = ((sum((Prec')))-1)/(NumberUser-1); % precison treshould


%% Telic Trust

UltiITN=zeros(NumberUser,NumberUser);

for i=1:NumberUser
    
    for j=1:NumberUser
        
        if i~=j
            
           if InITN(i,j)==1 && Prec(i,j) >= D(i)
              
              UltiITN(i,j) = 1; 
              
           end
        end
    end
end

 UltiTrust = (2.*(InTrust.*Prec)./(InTrust+Prec));

UT = ((sum((UltiTrust')))-1)/(NumberUser-1);


%% Recommendations and Evaluation
 
  %% Rating Prediction
Thr=4;
   UltiPred_TP = 0;
   UltiPred_TN = 0;
   UltiPred_FP = 0;
   UltiPred_FN = 0;

   UltiScor_TP = 0;
   UltiScor_TN = 0;
   UltiScor_FP = 0;
   UltiScor_FN = 0;

NumberUserTests = NumberUser;

UltiPredRating=zeros(NumberUserTests,NumberItem);   
UltiScoringItems= zeros(NumberUserTests,NumberItem);   

ItemsNum_Pred = 0;
AbsErrorRate_Pred = 0;
PowErrorRate_Pred = 0;

ItemsNum_Scor = 0;
AbsErrorRate_Scor = 0;
PowErrorRate_Scor = 0;

UltiPredCount = 0;
UltiScorCount = 0;

for i=1:NumberUserTests
    
    for k=1:NumberItem
        CCC=0; 
        
        UltiR=0;
        UltiSumPred=0;
        
        UltiMulScore=1;
        UltiSumScore=0;
        
        SumationScore = 0;
        for j=1:NumberUser
            
            if i~=j && DataMatrix(j,k) ~= 0 && DataMatrix(i,k) ~= 0
                
               if UltiITN(i,j) == 1 
                
                  UltiR = UltiR + (UltiTrust(i,j)*(DataMatrix(j,k)-DataMatrix(j,NumberItem+1)));
                  UltiSumPred = UltiSumPred + UltiTrust(i,j);
                
                  UltiMulScore = UltiMulScore * (UltiTrust(i,j) * DataMatrix(j,k));
                  UltiSumScore = UltiSumScore + (UltiTrust(i,j) * DataMatrix(j,k));
                  CCC = CCC+1;
                  
                  SumationScore = SumationScore + DataMatrix(j,k);
                  
               end            
            
            end
        end
        
        if UltiSumPred ~= 0
           UltiPredRating(i,k) = DataMatrix(i,NumberItem+1) + (UltiR / UltiSumPred);
           UltiPredCount = UltiPredCount + 1 ;
        else
            UltiPredRating(i,k) = DataMatrix(i,NumberItem+1);
            
        end
        
        if UltiSumScore ~= 0
            UltiScoringItems(i,k) = ((SumationScore/CCC) + (UltiMulScore / UltiSumScore)) / 2;
          
           UltiScorCount = UltiScorCount + 1 ;
        else
            UltiScoringItems(i,k) = 0;
        end
                
        %%%% evaluations of prediction
        TargetRating = DataMatrix(i,k);        
        UltiPred = UltiPredRating(i,k);
        UltiScore = UltiPredRating(i,k);
        
        
        if TargetRating ~= 0 && UltiPred > 0
       
           if TargetRating >= 3 && UltiPred >= Thr
             
               UltiPred_TP = UltiPred_TP + 1;
                
           elseif TargetRating < 3 && UltiPred >= Thr
               
               UltiPred_FP = UltiPred_FP + 1; 
                   
           elseif TargetRating >= 3 && UltiPred < Thr
                
               UltiPred_FN = UltiPred_FN + 1;
                
           elseif TargetRating < 3 && UltiPred < Thr
                
               UltiPred_TN = UltiPred_TN + 1;
                
           end
           
           
           
           % MAE & RMSE
           AbsErrorRate_Pred = AbsErrorRate_Pred + (abs(UltiPred - TargetRating));
           PowErrorRate_Pred = PowErrorRate_Pred + ((UltiPred - TargetRating)^2);      
           ItemsNum_Pred = ItemsNum_Pred + 1;
           
        end
               
    end
     
end

    UltiPred_Tot = UltiPred_TP + UltiPred_FP + UltiPred_TN + UltiPred_FN;
         
    UltiPred_Precision = (UltiPred_TP / (UltiPred_TP + UltiPred_FP)) * 100;  
    UltiPred_Recall = (UltiPred_TP / (UltiPred_TP + UltiPred_FN)) * 100;
    
    UltiPred_MAE = AbsErrorRate_Pred / ItemsNum_Pred;
    UltiPred_RMSE = sqrt(PowErrorRate_Pred / ItemsNum_Pred);
    UltiPred_Coverage = (UltiPredCount / NumberRates)*100;
      
    
%% Ranking Prediction

PredRankings = zeros(NumberUserTests,NumberItem);
ScoreRankings = zeros(NumberUserTests,NumberItem);

for i=1:NumberUserTests
    
    [Rat,UltiPredRanking]=sort(UltiPredRating(i,:),'descend');
    [Sco,UltiScorRanking]=sort(UltiScoringItems(i,:),'descend');
    
    SortedRate = 0;
    PrRanking = 1;
    
    SortedSco = 0;
    ScRanking = 1;
    
    for k=1:NumberItem
        
        if  SortedRate == Rat(1,k)
            
            SortedRate = Rat(1,k);
            PrItemIds = UltiPredRanking(1,k);            
            PredRankings(i,PrItemIds) = PrRanking;
            
        else 
            SortedRate = Rat(1,k);
            PrItemIds = UltiPredRanking(1,k);
            PrRanking = k;
            PredRankings(i,PrItemIds) = PrRanking;
        end
        
        if  SortedSco == Sco(1,k)
            
            SortedSco = Sco(1,k);
            ScItemIds = UltiScorRanking(1,k);            
            ScoreRankings(i,ScItemIds) = ScRanking;
            
        else 
            SortedSco = Sco(1,k);
            ScItemIds = UltiScorRanking(1,k);
            ScRanking = k;
            ScoreRankings(i,ScItemIds) = ScRanking;
        end

    end
end

 
FinalRankings = zeros(NumberUserTests,NumberItem);

for i=1:NumberUserTests
    
    for k=1:NumberItem
        
        FinalRankings(i,k) = ceil((PredRankings(i,k) + ScoreRankings(i,k)) / 2);
    end
end


%% Top k recomendation Evaluation

top_k = 9;

RecomendationPrecision=0;

RecommendationRecall=0;

U = 0;
for i=1:NumberUserTests
    
    ItemsCounts = sum(DataMatrix(i,:)>3);
    if ItemsCounts ==0
        ItemsCounts = 1;
    end

    [SOR,Recomendations] = sort(FinalRankings(i,:));
     
    Revelent=0;    
    
    for k=1:top_k
        
        ItemID = Recomendations(1,k);

        if DataMatrix(i,ItemID) >= 3
              
           Revelent = Revelent + 1;
               
        end
        
    end
    if Revelent~=0
        U = U+1;
    end

    RecomendationPrecision = RecomendationPrecision+(Revelent/top_k);
 
    RecommendationRecall = RecommendationRecall+(Revelent/ItemsCounts);

end



Recommendation_Precision = (RecomendationPrecision/U)*100;

Recommendation_Recall = (RecommendationRecall/U)*100;

F_Score = (2*(Recommendation_Precision*Recommendation_Recall))/(Recommendation_Precision+Recommendation_Recall);


  %% Results
  
   disp('Result of Ultimate ITN based Predicting :');
   disp('   ');
  % disp(['Accuracy  : ' num2str(UltiPred_Accuracy)]);
   disp(['Precision : ' num2str(UltiPred_Precision)]);
   disp(['Recall    : ' num2str(UltiPred_Recall)]);
   
   disp('   ');

   disp(['MAE  : ' num2str(UltiPred_MAE)]);
   disp(['RMSE : ' num2str(UltiPred_RMSE)]);
   disp(['Coverage : ' num2str(UltiPred_Coverage)]);    
   disp('   ');


   disp('Result of Ultimate ITN in Trustees identifying :');
      disp('   ');   
%     disp(['Precentage of Correct Trust Identificated  : ' num2str(PCTI)]);
%     disp(['Precentage of Correct Initial Trust Identificated  : ' num2str(InPCTI)]);
   disp('   ');
   
   
   
   disp('Result of Ultimate ITN in Recommendation :');
      disp('   ');
   
   disp(['Recommendation Precision  : ' num2str(Recommendation_Precision)]);

      disp('   ');

   disp(['Recommendation Recall  : ' num2str(Recommendation_Recall)]);
     
   disp('   ');

   disp(['Recommendation F_Score  : ' num2str(F_Score)]);
  
   disp('   ');
   disp('   ------------   ');
   disp('   ');
   
   disp(['Run Time :' num2str(toc)]);
   
  %% In this part Each measures of proposed similarity, confidence, and identical opinion is evaluated in prediction
NumberUserTests = NumberUser;
   Simi_TP = 0;
   Simi_TN = 0;
   Simi_FP = 0;
   Simi_FN = 0;

   Con_TP = 0;
   Con_TN = 0;
   Con_FP = 0;
   Con_FN = 0;

   Sam_TP = 0;
   Sam_TN = 0;
   Sam_FP = 0;
   Sam_FN = 0;
   
SimiPredRating=zeros(NumberUserTests,NumberItem);   
ConPredRating= zeros(NumberUserTests,NumberItem);   
SamPredRating= zeros(NumberUserTests,NumberItem); 

ItemsNumSimi = 0;
AbsErrorRateSimi = 0;
PowErrorRateSimi = 0;

ItemsNumCon = 0;
AbsErrorRateCon = 0;
PowErrorRateCon = 0;

ItemsNumSam = 0;
AbsErrorRateSam = 0;
PowErrorRateSam = 0;

SimiPredCount = 0;
ConPredCount = 0;
SamPredCount = 0;

Thr=4;
for i=1:NumberUserTests
    
    for k=1:NumberItem
        
        SimiR=0;
        SimiT=0;

        ConR=0;
        ConT=0;

        SamR=0;
        SamT=0;
                
        for j=1:NumberUser
            
            if i~=j && DataMatrix(j,k) ~= 0 && DataMatrix(i,k) ~= 0
                
               if Similarity(i,j) > 0 
                
                  SimiR = SimiR + (Similarity(i,j)*(DataMatrix(j,k)-DataMatrix(j,NumberItem+1)));
                  SimiT = SimiT + Similarity(i,j);
                
               end            
            
               if Confidence(i,j) > 0 
                
                  ConR = ConR + (Confidence(i,j)*(DataMatrix(j,k)-DataMatrix(j,NumberItem+1)));
                  ConT = ConT + Confidence(i,j);
                
               end 
               
               if SameOpinion(i,j) > 0 
                
                  SamR = SamR + (SameOpinion(i,j)*(DataMatrix(j,k)-DataMatrix(j,NumberItem+1)));
                  SamT = SamT + SameOpinion(i,j);
                
               end 
               
            end
        end
        
        if SimiT ~=0
           SimiPredRating(i,k) = DataMatrix(i,NumberItem+1) + (SimiR / SimiT);
           SimiPredCount = SimiPredCount + 1 ;
        else
            SimiPredRating(i,k) = DataMatrix(i,NumberItem+1);
            %SimiPredRating(i,k) = 0;
        end
        
        if ConT ~=0
           ConPredRating(i,k) = DataMatrix(i,NumberItem+1) + (ConR / ConT);
           ConPredCount = ConPredCount + 1 ;
        else
            ConPredRating(i,k) = DataMatrix(i,NumberItem+1);
            %ConPredRating(i,k) = 0;
        end
        
        if SamT ~=0
           SamPredRating(i,k) = DataMatrix(i,NumberItem+1) + (SamR / SamT);
           SamPredCount = SamPredCount + 1 ;
        else
            SamPredRating(i,k) = DataMatrix(i,NumberItem+1);
            %SamPredRating(i,k) = 0;
        end
        
        
        %%%% evaluations
        TargetRating = DataMatrix(i,k); 
        
        SimiPred = SimiPredRating(i,k);
        
        ConPred = ConPredRating(i,k);
        
        SamPred = SamPredRating(i,k);
        
        %%%similarity
        if TargetRating ~= 0 && SimiPred > 0
       
           if TargetRating >= 3 && SimiPred >= Thr
             
               Simi_TP = Simi_TP + 1;
                
           elseif TargetRating < 3 && SimiPred >= Thr
               
               Simi_FP = Simi_FP + 1; 
                   
           elseif TargetRating >= 3 && SimiPred < Thr
                
               Simi_FN = Simi_FN + 1;
                
           elseif TargetRating < 3 && SimiPred < Thr
                
               Simi_TN = Simi_TN + 1;
                
           end
           
           % MAE & RMSE
           AbsErrorRateSimi = AbsErrorRateSimi + (abs(SimiPred - TargetRating));
           PowErrorRateSimi = PowErrorRateSimi + ((SimiPred - TargetRating)^2);      
           ItemsNumSimi = ItemsNumSimi + 1;
           
        end
        
        %%%%Confidence
        if TargetRating ~= 0 && ConPred > 0
       
           if TargetRating >= 3 && ConPred >= Thr
             
               Con_TP = Con_TP + 1;
                
           elseif TargetRating < 3 && ConPred >= Thr
               
               Con_FP = Con_FP + 1; 
                   
           elseif TargetRating >= 3 && ConPred < Thr
                
               Con_FN = Con_FN + 1;
                
           elseif TargetRating < 3 && ConPred < Thr
                
               Con_TN = Con_TN + 1;
                
           end
           
           % MAE & RMSE
           AbsErrorRateCon = AbsErrorRateCon + (abs(ConPred - TargetRating));
           PowErrorRateCon = PowErrorRateCon + ((ConPred - TargetRating)^2);      
           ItemsNumCon = ItemsNumCon + 1;
           
        end
 
        %%%%SameOpinion
        if TargetRating ~= 0 && SamPred > 0
       
           if TargetRating >= 3 && SamPred >= Thr
             
               Sam_TP = Sam_TP + 1;
                
           elseif TargetRating < 3 && SamPred >= Thr
               
               Sam_FP = Sam_FP + 1; 
                   
           elseif TargetRating >= 3 && SamPred < Thr
                
               Sam_FN = Sam_FN + 1;
                
           elseif TargetRating < 3 && SamPred < Thr
                
               Sam_TN = Sam_TN + 1;
                
           end
           
           % MAE & RMSE
           AbsErrorRateSam = AbsErrorRateSam + (abs(SamPred - TargetRating));
           PowErrorRateSam = PowErrorRateSam + ((SamPred - TargetRating)^2);      
           ItemsNumSam = ItemsNumSam + 1;
           
        end
            
    end
     
end

% Evaluatation
 
    SimiTot = Simi_TP + Simi_FP + Simi_TN + Simi_FN;
    
    SimiAccuracy = ((Simi_TP + Simi_TN) / SimiTot) * 100;     
    SimiPrecision = (Simi_TP / (Simi_TP + Simi_FP)) * 100;  
    SimiRecall = (Simi_TP / (Simi_TP + Simi_FN)) * 100;
    
    MAE_Simi = AbsErrorRateSimi / ItemsNumSimi;
    RMSE_Simi = sqrt(PowErrorRateSimi / ItemsNumSimi);
    SimiPrec = 1 - (MAE_Simi / (5 - 1) ) ;
    
    %%%%
    ConTot = Con_TP + Con_FP + Con_TN + Con_FN;
    
    ConAccuracy = ((Con_TP + Con_TN) / ConTot) * 100;     
    ConPrecision = (Con_TP / (Con_TP + Con_FP)) * 100;  
    ConRecall = (Con_TP / (Con_TP + Con_FN)) * 100;
    
    MAE_Con = AbsErrorRateCon / ItemsNumCon;
    RMSE_Con = sqrt(PowErrorRateCon / ItemsNumCon);
    ConPrec = 1 - (MAE_Con / (5 - 1) ) ;

    %%%%
    SamTot = Sam_TP + Sam_FP + Sam_TN + Sam_FN;
    
    SamAccuracy = ((Sam_TP + Sam_TN) / SamTot) * 100;     
    SamPrecision = (Sam_TP / (Sam_TP + Sam_FP)) * 100;  
    SamRecall = (Sam_TP / (Sam_TP + Sam_FN)) * 100;
    
    MAE_Sam = AbsErrorRateSam / ItemsNumSam;
    RMSE_Sam = sqrt(PowErrorRateSam / ItemsNumSam);
    SamPrec = 1 - (MAE_Sam / (5 - 1) ) ;
    
   
    SimiCoverage = (SimiPredCount / NumberRates)*100;
    ConCoverage = (ConPredCount / NumberRates)*100;
    SamCoverage = (SamPredCount / NumberRates)*100;
  %% Results
  
   disp('Result of Similarity :');
   disp('   ');
   disp(['Accuracy  : ' num2str(SimiAccuracy)]);
   disp(['Precision : ' num2str(SimiPrecision)]);
   disp(['Recall    : ' num2str(SimiRecall)]);
   
   disp('   ');

   disp(['MAE  : ' num2str(MAE_Simi)]);
   disp(['RMSE : ' num2str(RMSE_Simi)]);
   disp(['Coverage : ' num2str(SimiCoverage)]);    
   disp(['Precision : ' num2str(SimiPrec)]);
   
   disp('   ');
   disp('   ------------   ');
   disp('   ');
   
   disp('Result of Confidence :');
   disp('   ');
   disp(['Accuracy  : ' num2str(ConAccuracy)]);
   disp(['Precision : ' num2str(ConPrecision)]);
   disp(['Recall    : ' num2str(ConRecall)]);
   
   disp('   ');

   disp(['MAE  : ' num2str(MAE_Con)]);
   disp(['RMSE : ' num2str(RMSE_Con)]);
   disp(['Coverage : ' num2str(ConCoverage)]);   
   disp(['Precision : ' num2str(ConPrec)]);
   
   disp('   ');
   disp('   ------------   ');
   disp('   ');
   

   disp('Result of Same Opinion :');
   disp('   ');
   disp(['Accuracy  : ' num2str(SamAccuracy)]);
   disp(['Precision : ' num2str(SamPrecision)]);
   disp(['Recall    : ' num2str(SamRecall)]);
   
   disp('   ');

   disp(['MAE  : ' num2str(MAE_Sam)]);
   disp(['RMSE : ' num2str(RMSE_Sam)]);
   disp(['Coverage : ' num2str(SamCoverage)]);   
   disp(['Precision : ' num2str(SamPrec)]);
   
   disp('   ');
   disp('   ------------   ');
   disp('   ');
   

  
  
  