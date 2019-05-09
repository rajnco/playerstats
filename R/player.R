library(R6)

PlayerIndex <- R6::R6Class("PlayerIndex", 
	 public = list(
	 	initialize = function(id=0, name="", country=""){
	 		stopifnot(is.character(name), length(name) == 1)
	 		stopifnot(is.double(id), length(id)==1)
	 		stopifnot(is.character(country), length(country) == 1)
	 		private$id <- id
	 		private$name <- name
	 		private$country <- country
	 	},
	 	finalize = function(){
	 		id = 0
	 		name = ""
	 		country = ""
			print(paste(" finalize from Player Index ", " "))	 		
	 	}
	 ),
	 private = list(
	 	id = 0,
	 	name = "",
	 	country = ""
	 )
)

PlayerBasic <- R6::R6Class("PlayerBasic",
	inherit = PlayerIndex,
	public = list(
		initialize = function(id, name, country, fullname, dob_place, age, role, batting_style, bowling_style){
			super$initialize(id, name, country)
			private$fullname = fullname 
			private$dob_place = dob_place 
			private$age = age
			private$role = role 
			private$batting_style = batting_style 
			private$bowling_style = bowling_style
		},
		finalize = function() {
			id = 0
			name = ""
			country = ""
			fullname = "" 
			dob_place = "" 
			age = ""
			role = "" 
			batting_style = "" 
			bowling_style = ""
			print(paste("Finalize from Player Basic", " "))
		}
	),
	private  = list(
		id = 0,
		name = "",
		country = "",
		fullname = "", 
		dob_place = "", 
		age = "", 
		role = "", 
		batting_style = "", 
		bowling_style = ""
	)
)

PlayerFull <- R6::R6Class("PlayerFull",
	inherit = PlayerBasic,
	public = list(
		initialize = function(id, name, country, fullname, dob_place, age, role, batting_style, bowling_style){
			super$initialize(id, name, country, fullname, dob_place, age, role, batting_style, bowling_style)
			private$Test_Batting_Mat = Test_Batting_Mat; private$Test_Batting_Inns = Test_Batting_Inns; private$Test_Batting_NO = Test_Batting_NO; private$Test_Batting_Runs = Test_Batting_Runs; private$Test_Batting_HS = Test_Batting_HS; private$Test_Batting_Ave = Test_Batting_Ave; private$Test_Batting_BF = Test_Batting_BF; private$Test_Batting_SR = Test_Batting_SR; private$Test_Batting_100 = Test_Batting_100; private$Test_Batting_50 = Test_Batting_50; private$Test_Batting_4s = Test_Batting_4s; private$Test_Batting_6s = Test_Batting_6s; private$Test_Batting_Ct = Test_Batting_Ct; private$Test_Batting_St = Test_Batting_St;
			private$ODI_Batting_Mat = ODI_Batting_Mat; private$ODI_Batting_Inns = ODI_Batting_Inns; private$ODI_Batting_NO = ODI_Batting_NO; private$ODI_Batting_Runs = ODI_Batting_Runs; private$ODI_Batting_HS = ODI_Batting_HS; private$ODI_Batting_Ave = ODI_Batting_Ave; private$ODI_Batting_BF = ODI_Batting_BF; private$ODI_Batting_SR = ODI_Batting_SR; private$ODI_Batting_100 = ODI_Batting_100; private$ODI_Batting_50 = ODI_Batting_50; private$ODI_Batting_4s = ODI_Batting_4s; private$ODI_Batting_6s = ODI_Batting_6s; private$ODI_Batting_Ct = ODI_Batting_Ct; private$ODI_Batting_St = ODI_Batting_St;
			private$T20I_Batting_Mat = T20I_Batting_Mat; private$T20I_Batting_Inns = T20I_Batting_Inns; private$T20I_Batting_NO = T20I_Batting_NO; private$T20I_Batting_Runs = T20I_Batting_Runs; private$T20I_Batting_HS = T20I_Batting_HS; private$T20I_Batting_Ave = T20I_Batting_Ave; private$T20I_Batting_BF = T20I_Batting_BF; private$T20I_Batting_SR = T20I_Batting_SR; private$T20I_Batting_100 = T20I_Batting_100; private$T20I_Batting_50 = T20I_Batting_50; private$T20I_Batting_4s = T20I_Batting_4s; private$T20I_Batting_6s = T20I_Batting_6s; private$T20I_Batting_Ct = T20I_Batting_Ct; private$T20I_Batting_St = T20I_Batting_St;
			private$Firstcls_Batting_Mat = Firstcls_Batting_Mat; private$Firstcls_Batting_Inns = Firstcls_Batting_Inns; private$Firstcls_Batting_NO = Firstcls_Batting_NO; private$Firstcls_Batting_Runs = Firstcls_Batting_Runs; private$Firstcls_Batting_HS = Firstcls_Batting_HS; private$Firstcls_Batting_Ave = Firstcls_Batting_Ave; private$Firstcls_Batting_BF = Firstcls_Batting_BF; private$Firstcls_Batting_SR = Firstcls_Batting_SR; private$Firstcls_Batting_100 = Firstcls_Batting_100; private$Firstcls_Batting_50 = Firstcls_Batting_50; private$Firstcls_Batting_4s = Firstcls_Batting_4s; private$Firstcls_Batting_6s = Firstcls_Batting_6s; private$Firstcls_Batting_Ct = Firstcls_Batting_Ct; private$Firstcls_Batting_St = Firstcls_Batting_St;
			private$ListA_Batting_Mat = ListA_Batting_Mat;private$ListA_Batting_Inns = ListA_Batting_Inns; private$ListA_Batting_NO = ListA_Batting_NO; private$ListA_Batting_Runs = ListA_Batting_Runs; private$ListA_Batting_HS = ListA_Batting_HS; private$ListA_Batting_Ave = ListA_Batting_Ave; private$ListA_Batting_BF = ListA_Batting_BF; private$ListA_Batting_SR = ListA_Batting_SR; private$ListA_Batting_100 = ListA_Batting_100; private$ListA_Batting_50 = ListA_Batting_50; private$ListA_Batting_4s = ListA_Batting_4s; private$ListA_Batting_6s = ListA_Batting_6s; private$ListA_Batting_Ct = ListA_Batting_Ct; private$ListA_Batting_St = ListA_Batting_St;
			private$T20S_Batting_Mat = T20S_Batting_Mat; private$T20S_Batting_Inns = T20S_Batting_Inns; private$T20S_Batting_NO = T20S_Batting_NO; private$T20S_Batting_Runs = T20S_Batting_Runs; private$T20S_Batting_HS = T20S_Batting_HS; private$T20S_Batting_Ave = T20S_Batting_Ave; private$T20S_Batting_BF = T20S_Batting_BF; private$T20S_Batting_SR = T20S_Batting_SR; private$T20S_Batting_100 = T20S_Batting_100; private$T20S_Batting_50 = T20S_Batting_50; private$T20S_Batting_4s = T20S_Batting_4s; private$T20S_Batting_6s = T20S_Batting_6s; private$T20S_Batting_Ct = T20S_Batting_Ct; private$T20S_Batting_St = T20S_Batting_St;
			private$Test_Bowling_Mat = Test_Bowling_Mat; private$Test_Bowling_Inns = Test_Bowling_Inns; private$Test_Bowling_Balls = Test_Bowling_Balls; private$Test_Bowling_Runs = Test_Bowling_Runs; private$Test_Bowling_Wkts = Test_Bowling_Wkts; private$Test_Bowling_BBI = Test_Bowling_BBI;private$Test_Bowling_BBM = Test_Bowling_BBM; private$Test_Bowling_Ave = Test_Bowling_Ave; private$Test_Bowling_Econ = Test_Bowling_Econ; private$Test_Bowling_SR = Test_Bowling_SR; private$Test_Bowling_4w = Test_Bowling_4w; private$Test_Bowling_5w = Test_Bowling_5w; private$Test_Bowling_10 = Test_Bowling_10;
			private$ODI_Bowling_Mat = ODI_Bowling_Mat; private$ODI_Bowling_Inns = ODI_Bowling_Inns; private$ODI_Bowling_Balls = ODI_Bowling_Balls; private$ODI_Bowling_Runs = ODI_Bowling_Runs; private$ODI_Bowling_Wkts = ODI_Bowling_Wkts; private$ODI_Bowling_BBI = ODI_Bowling_BBI; private$ODI_Bowling_BBM = ODI_Bowling_BBM; private$ODI_Bowling_Ave = ODI_Bowling_Ave;private$ODI_Bowling_Econ = ODI_Bowling_Econ; private$ODI_Bowling_SR = ODI_Bowling_SR; private$ODI_Bowling_4w = ODI_Bowling_4w; private$ODI_Bowling_5w = ODI_Bowling_5w; private$ODI_Bowling_10 = ODI_Bowling_10; 
			private$T20I_Bowling_Mat = T20I_Bowling_Mat; private$T20I_Bowling_Inns = T20I_Bowling_Inns; private$T20I_Bowling_Balls = T20I_Bowling_Balls; private$T20I_Bowling_Runs = T20I_Bowling_Runs; private$T20I_Bowling_Wkts = T20I_Bowling_Wkts; private$T20I_Bowling_BBI = T20I_Bowling_BBI; private$T20I_Bowling_BBM = T20I_Bowling_BBM; private$T20I_Bowling_Ave = T20I_Bowling_Ave; private$T20I_Bowling_Econ = T20I_Bowling_Econ; private$T20I_Bowling_SR = T20I_Bowling_SR; private$T20I_Bowling_4w = T20I_Bowling_4w; private$T20I_Bowling_5w = T20I_Bowling_5w; private$T20I_Bowling_10 = T20I_Bowling_10;			
			private$Firstcls_Bowling_Mat = Firstcls_Bowling_Mat; private$Firstcls_Bowling_Inns = Firstcls_Bowling_Inns; private$Firstcls_Bowling_Balls = Firstcls_Bowling_Balls; private$Firstcls_Bowling_Runs = Firstcls_Bowling_Runs; private$Firstcls_Bowling_Wkts = Firstcls_Bowling_Wkts; private$Firstcls_Bowling_BBI = Firstcls_Bowling_BBI; private$Firstcls_Bowling_BBM = Firstcls_Bowling_BBM; private$Firstcls_Bowling_Ave = Firstcls_Bowling_Ave; private$Firstcls_Bowling_Econ = Firstcls_Bowling_Econ; private$Firstcls_Bowling_SR = Firstcls_Bowling_SR; private$Firstcls_Bowling_4w = Firstcls_Bowling_4w; private$Firstcls_Bowling_5w = Firstcls_Bowling_5w; private$Firstcls_Bowling_10 = Firstcls_Bowling_10;
			private$ListA_Bowling_Mat = ListA_Bowling_Mat; private$ListA_Bowling_Inns = ListA_Bowling_Inns; private$ListA_Bowling_Balls = ListA_Bowling_Balls; private$ListA_Bowling_Runs = ListA_Bowling_Runs;private$ListA_Bowling_Wkts = ListA_Bowling_Wkts; private$ListA_Bowling_BBI = ListA_Bowling_BBI; private$ListA_Bowling_BBM = ListA_Bowling_BBM; private$ListA_Bowling_Ave = ListA_Bowling_Ave;private$ListA_Bowling_Econ = ListA_Bowling_Econ; private$ListA_Bowling_SR = ListA_Bowling_SR; private$ListA_Bowling_4w = ListA_Bowling_4w; private$ListA_Bowling_5w = ListA_Bowling_5w; private$ListA_Bowling_10 = ListA_Bowling_10;
			private$T20S_Bowling_Mat = T20S_Bowling_Mat; private$T20S_Bowling_Inns = T20S_Bowling_Inns; private$T20S_Bowling_Balls = T20S_Bowling_Balls; private$T20S_Bowling_Runs = T20S_Bowling_Runs; private$T20S_Bowling_Wkts = T20S_Bowling_Wkts; private$T20S_Bowling_BBI = T20S_Bowling_BBI; private$T20S_Bowling_BBM = T20S_Bowling_BBM; private$T20S_Bowling_Ave = T20S_Bowling_Ave; private$T20S_Bowling_Econ = T20S_Bowling_Econ; private$T20S_Bowling_SR = T20S_Bowling_SR; private$T20S_Bowling_4w = T20S_Bowling_4w; private$T20S_Bowling_5w = T20S_Bowling_5w; private$T20S_Bowling_10 = T20S_Bowling_10			
		},
		finalize = function(){
			id="";name="";country="";fullname="";dob_place="";age="";role="";batting_style="";bowling_style="";
			Test_Batting_Mat="";Test_Batting_Inns="";Test_Batting_NO="";Test_Batting_Runs="";Test_Batting_HS="";Test_Batting_Ave="";Test_Batting_BF="";Test_Batting_SR="";Test_Batting_100="";Test_Batting_50="";Test_Batting_4s="";Test_Batting_6s="";Test_Batting_Ct="";Test_Batting_St="";
			ODI_Batting_Mat="";ODI_Batting_Inns="";ODI_Batting_NO="";ODI_Batting_Runs="";ODI_Batting_HS="";ODI_Batting_Ave="";ODI_Batting_BF="";ODI_Batting_SR="";ODI_Batting_100="";ODI_Batting_50="";ODI_Batting_4s="";ODI_Batting_6s="";ODI_Batting_Ct="";ODI_Batting_St="";
			T20I_Batting_Mat="";T20I_Batting_Inns="";T20I_Batting_NO="";T20I_Batting_Runs="";T20I_Batting_HS="";T20I_Batting_Ave="";T20I_Batting_BF="";T20I_Batting_SR="";T20I_Batting_100="";T20I_Batting_50="";T20I_Batting_4s="";T20I_Batting_6s="";T20I_Batting_Ct="";T20I_Batting_St="";
			Firstcls_Batting_Mat="";Firstcls_Batting_Inns="";Firstcls_Batting_NO="";Firstcls_Batting_Runs="";Firstcls_Batting_HS="";Firstcls_Batting_Ave="";Firstcls_Batting_BF="";Firstcls_Batting_SR="";Firstcls_Batting_100="";Firstcls_Batting_50="";Firstcls_Batting_4s="";Firstcls_Batting_6s="";Firstcls_Batting_Ct="";Firstcls_Batting_St="";
			ListA_Batting_Mat="";ListA_Batting_Inns="";ListA_Batting_NO="";ListA_Batting_Runs="";ListA_Batting_HS="";ListA_Batting_Ave="";ListA_Batting_BF="";ListA_Batting_SR="";ListA_Batting_100="";ListA_Batting_50="";ListA_Batting_4s="";ListA_Batting_6s="";ListA_Batting_Ct="";ListA_Batting_St="";
			T20S_Batting_Mat="";T20S_Batting_Inns="";T20S_Batting_NO="";T20S_Batting_Runs="";T20S_Batting_HS="";T20S_Batting_Ave="";T20S_Batting_BF="";T20S_Batting_SR="";T20S_Batting_100="";T20S_Batting_50="";T20S_Batting_4s="";T20S_Batting_6s="";T20S_Batting_Ct="";T20S_Batting_St="";Test_Bowling_Mat="";Test_Bowling_Inns="";Test_Bowling_Balls="";Test_Bowling_Runs="";Test_Bowling_Wkts="";Test_Bowling_BBI="";Test_Bowling_BBM="";Test_Bowling_Ave="";Test_Bowling_Econ="";Test_Bowling_SR="";Test_Bowling_4w="";Test_Bowling_5w="";Test_Bowling_10="";ODI_Bowling_Mat="";ODI_Bowling_Inns="";ODI_Bowling_Balls="";ODI_Bowling_Runs="";ODI_Bowling_Wkts="";ODI_Bowling_BBI="";ODI_Bowling_BBM="";ODI_Bowling_Ave="";ODI_Bowling_Econ="";ODI_Bowling_SR="";ODI_Bowling_4w="";ODI_Bowling_5w="";ODI_Bowling_10="";T20I_Bowling_Mat="";T20I_Bowling_Inns="";T20I_Bowling_Balls="";T20I_Bowling_Runs="";T20I_Bowling_Wkts="";T20I_Bowling_BBI="";T20I_Bowling_BBM="";T20I_Bowling_Ave="";T20I_Bowling_Econ="";T20I_Bowling_SR="";T20I_Bowling_4w="";T20I_Bowling_5w="";T20I_Bowling_10="";Firstcls_Bowling_Mat="";Firstcls_Bowling_Inns="";Firstcls_Bowling_Balls="";Firstcls_Bowling_Runs="";Firstcls_Bowling_Wkts="";Firstcls_Bowling_BBI="";Firstcls_Bowling_BBM="";Firstcls_Bowling_Ave="";Firstcls_Bowling_Econ="";Firstcls_Bowling_SR="";Firstcls_Bowling_4w="";Firstcls_Bowling_5w="";Firstcls_Bowling_10="";ListA_Bowling_Mat="";ListA_Bowling_Inns="";ListA_Bowling_Balls="";ListA_Bowling_Runs="";ListA_Bowling_Wkts="";ListA_Bowling_BBI="";ListA_Bowling_BBM="";ListA_Bowling_Ave="";ListA_Bowling_Econ="";ListA_Bowling_SR="";ListA_Bowling_4w="";ListA_Bowling_5w="";ListA_Bowling_10="";T20S_Bowling_Mat="";T20S_Bowling_Inns="";T20S_Bowling_Balls="";T20S_Bowling_Runs="";T20S_Bowling_Wkts="";T20S_Bowling_BBI="";T20S_Bowling_BBM="";T20S_Bowling_Ave="";T20S_Bowling_Econ="";T20S_Bowling_SR="";T20S_Bowling_4w="";T20S_Bowling_5w="";T20S_Bowling_10=""
		}	
	),
	private = list(
		id="",name="",country="",fullname="",dob_place="",age="",role="",batting_style="",bowling_style="",
		Test_Batting_Mat="",Test_Batting_Inns="",Test_Batting_NO="",Test_Batting_Runs="",Test_Batting_HS="",Test_Batting_Ave="",Test_Batting_BF="",Test_Batting_SR="",Test_Batting_100="",Test_Batting_50="",Test_Batting_4s="",Test_Batting_6s="",Test_Batting_Ct="",Test_Batting_St="",
		ODI_Batting_Mat="",ODI_Batting_Inns="",ODI_Batting_NO="",ODI_Batting_Runs="",ODI_Batting_HS="",ODI_Batting_Ave="",ODI_Batting_BF="",ODI_Batting_SR="",ODI_Batting_100="",ODI_Batting_50="",ODI_Batting_4s="",ODI_Batting_6s="",ODI_Batting_Ct="",ODI_Batting_St="",
		T20I_Batting_Mat="",T20I_Batting_Inns="",T20I_Batting_NO="",T20I_Batting_Runs="",T20I_Batting_HS="",T20I_Batting_Ave="",T20I_Batting_BF="",T20I_Batting_SR="",T20I_Batting_100="",T20I_Batting_50="",T20I_Batting_4s="",T20I_Batting_6s="",T20I_Batting_Ct="",T20I_Batting_St="",
		Firstcls_Batting_Mat="",Firstcls_Batting_Inns="",Firstcls_Batting_NO="",Firstcls_Batting_Runs="",Firstcls_Batting_HS="",Firstcls_Batting_Ave="",Firstcls_Batting_BF="",Firstcls_Batting_SR="",Firstcls_Batting_100="",Firstcls_Batting_50="",Firstcls_Batting_4s="",Firstcls_Batting_6s="",Firstcls_Batting_Ct="",Firstcls_Batting_St="",
		ListA_Batting_Mat="",ListA_Batting_Inns="",ListA_Batting_NO="",ListA_Batting_Runs="",ListA_Batting_HS="",ListA_Batting_Ave="",ListA_Batting_BF="",ListA_Batting_SR="",ListA_Batting_100="",ListA_Batting_50="",ListA_Batting_4s="",ListA_Batting_6s="",ListA_Batting_Ct="",ListA_Batting_St="",
		T20S_Batting_Mat="",T20S_Batting_Inns="",T20S_Batting_NO="",T20S_Batting_Runs="",T20S_Batting_HS="",T20S_Batting_Ave="",T20S_Batting_BF="",T20S_Batting_SR="",T20S_Batting_100="",T20S_Batting_50="",T20S_Batting_4s="",T20S_Batting_6s="",T20S_Batting_Ct="",T20S_Batting_St="",Test_Bowling_Mat="",Test_Bowling_Inns="",Test_Bowling_Balls="",Test_Bowling_Runs="",Test_Bowling_Wkts="",Test_Bowling_BBI="",Test_Bowling_BBM="",Test_Bowling_Ave="",Test_Bowling_Econ="",Test_Bowling_SR="",Test_Bowling_4w="",Test_Bowling_5w="",Test_Bowling_10="",ODI_Bowling_Mat="",ODI_Bowling_Inns="",ODI_Bowling_Balls="",ODI_Bowling_Runs="",ODI_Bowling_Wkts="",ODI_Bowling_BBI="",ODI_Bowling_BBM="",ODI_Bowling_Ave="",ODI_Bowling_Econ="",ODI_Bowling_SR="",ODI_Bowling_4w="",ODI_Bowling_5w="",ODI_Bowling_10="",T20I_Bowling_Mat="",T20I_Bowling_Inns="",T20I_Bowling_Balls="",T20I_Bowling_Runs="",T20I_Bowling_Wkts="",T20I_Bowling_BBI="",T20I_Bowling_BBM="",T20I_Bowling_Ave="",T20I_Bowling_Econ="",T20I_Bowling_SR="",T20I_Bowling_4w="",T20I_Bowling_5w="",T20I_Bowling_10="",Firstcls_Bowling_Mat="",Firstcls_Bowling_Inns="",Firstcls_Bowling_Balls="",Firstcls_Bowling_Runs="",Firstcls_Bowling_Wkts="",Firstcls_Bowling_BBI="",Firstcls_Bowling_BBM="",Firstcls_Bowling_Ave="",Firstcls_Bowling_Econ="",Firstcls_Bowling_SR="",Firstcls_Bowling_4w="",Firstcls_Bowling_5w="",Firstcls_Bowling_10="",ListA_Bowling_Mat="",ListA_Bowling_Inns="",ListA_Bowling_Balls="",ListA_Bowling_Runs="",ListA_Bowling_Wkts="",ListA_Bowling_BBI="",ListA_Bowling_BBM="",ListA_Bowling_Ave="",ListA_Bowling_Econ="",ListA_Bowling_SR="",ListA_Bowling_4w="",ListA_Bowling_5w="",ListA_Bowling_10="",T20S_Bowling_Mat="",T20S_Bowling_Inns="",T20S_Bowling_Balls="",T20S_Bowling_Runs="",T20S_Bowling_Wkts="",T20S_Bowling_BBI="",T20S_Bowling_BBM="",T20S_Bowling_Ave="",T20S_Bowling_Econ="",T20S_Bowling_SR="",T20S_Bowling_4w="",T20S_Bowling_5w="",T20S_Bowling_10=""
	)
)
