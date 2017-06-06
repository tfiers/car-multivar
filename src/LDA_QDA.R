source('PCA.R')

LDA_model = lda(euro_standard~., XTrain_PCA_and_response)

print(get_error_rate(LDA_model, XTrain_PCA_and_response, type='da'))
print(get_error_rate(LDA_model, XTest_PCA_and_response, type='da'))


QDA_model = qda(euro_standard~., XTrain_PCA_and_response)

print(get_error_rate(QDA_model, XTrain_PCA_and_response, type='da'))
print(get_error_rate(QDA_model, XTest_PCA_and_response, type='da'))
