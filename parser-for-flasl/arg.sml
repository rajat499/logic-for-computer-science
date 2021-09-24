val ast1 = HENCE([COND (ATOM("prices rise") , ATOM("the poor and the salaried class will be unhappy")), 
COND (ATOM("taxes are increased") , ATOM("the businessmen will be unhappy")), 
COND (OR (ATOM("the poor and the salaried class are unhappy") , ATOM("the businessmen are unhappy")) , NOT (ATOM("the Government will be re-elected"))), 
COND (ATOM("Government expenditure exceeds its revenue") , ATOM("Inflation rises")), 
COND (NOT (OR (OR (ATOM("taxes are increased") , ATOM("the Governmentresorts to deficit financing")) , ATOM("the Government takes a loan"))) , ATOM("Government expenditure willexceed its revenue")), 
COND (ATOM("the Government resorts to deficit financing") , ATOM("inflation rises")), 
COND (ATOM("inflationrises") , ATOM("prices rise")), 
ATOM("the Government will be re-elected")], ATOM("the Government takesa loan"));