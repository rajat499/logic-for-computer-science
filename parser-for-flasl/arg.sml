HENCE([IF (ATOM("the poor and the salaried class will be unhappy") , ATOM("prices rise")), 
IF (ATOM("the businessmen will be unhappy") , ATOM("taxes are increased")), 
IF (NOT (ATOM("the Government will be re-elected")) , OR (ATOM("the poor and the salaried class are unhappy") , ATOM("the businessmen are unhappy"))), 
IF (ATOM("Inflation rises") , ATOM("Government expenditure exceeds its revenue")), 
IF (ATOM("Government expenditure willexceed its revenue") , NOT (OR (OR (ATOM("taxes are increased") , ATOM("the Governmentresorts to deficit financing")) , ATOM("the Government takes a loan")))), 
IF (ATOM("inflation rises") , ATOM("the Government resorts to deficit financing")), 
IF (ATOM("prices rise") , ATOM("inflationrises")), 
ATOM("the Government will be re-elected"), 
], ATOM("the Government takesa loan"))