survey.gen<-function(num.subj, num.item, num.resp){	
	#if(num.resp>=3&num.resp<=10){
	if(num.resp==3|num.resp==4|num.resp==5|num.resp==6|num.resp==7|num.resp==8|num.resp==9|num.resp==10){
	theta<-rnorm(num.subj)
	a<-runif(num.item,0.5,1.0)
	b<-rnorm(num.item,-1,.5)	
	CBRF1<-matrix(0, nrow=num.subj, ncol=num.item)
	CBRF2<-matrix(0, nrow=num.subj, ncol=num.item)
	CBRF3<-matrix(0, nrow=num.subj, ncol=num.item)
	CBRF4<-matrix(0, nrow=num.subj, ncol=num.item)
	CBRF5<-matrix(0, nrow=num.subj, ncol=num.item)
	CBRF6<-matrix(0, nrow=num.subj, ncol=num.item)
	CBRF7<-matrix(0, nrow=num.subj, ncol=num.item)
	CBRF8<-matrix(0, nrow=num.subj, ncol=num.item)
	CBRF9<-matrix(0, nrow=num.subj, ncol=num.item)
	CRF1<-matrix(0, nrow=num.subj, ncol=num.item)
	CRF2<-matrix(0, nrow=num.subj, ncol=num.item)
	CRF3<-matrix(0, nrow=num.subj, ncol=num.item)
	CRF4<-matrix(0, nrow=num.subj, ncol=num.item)
	CRF5<-matrix(0, nrow=num.subj, ncol=num.item)
	CRF6<-matrix(0, nrow=num.subj, ncol=num.item)
	CRF7<-matrix(0, nrow=num.subj, ncol=num.item)
	CRF8<-matrix(0, nrow=num.subj, ncol=num.item)
	CRF9<-matrix(0, nrow=num.subj, ncol=num.item)
	CRF10<-matrix(0, nrow=num.subj, ncol=num.item)
	response1<-matrix(0, nrow=num.subj, ncol=num.item)
	response2<-matrix(0, nrow=num.subj, ncol=num.item)
	response3<-matrix(0, nrow=num.subj, ncol=num.item)
	response4<-matrix(0, nrow=num.subj, ncol=num.item)
	response5<-matrix(0, nrow=num.subj, ncol=num.item)
	response6<-matrix(0, nrow=num.subj, ncol=num.item)
	response7<-matrix(0, nrow=num.subj, ncol=num.item)
	response8<-matrix(0, nrow=num.subj, ncol=num.item)
	response9<-matrix(0, nrow=num.subj, ncol=num.item)
	response10<-matrix(0, nrow=num.subj, ncol=num.item)
	Scale<-matrix(0, nrow=num.subj, ncol=num.item)
	   
	   if(num.resp==3){
	   		b1<-b
    		b2<-b1+.5  		
			for(i in 1:num.item){
    			CBRF1[,i]<-1/(1+exp(-1.702*a[i]*(theta-b1[i])))
        		CBRF2[,i]<-1/(1+exp(-1.702*a[i]*(theta-b2[i])))
        		CRF1[,i]<-1.0-CBRF1[,i]          
        		CRF2[,i]<-CBRF1[,i]-CBRF2[,i] 
        		CRF3[,i]<-CBRF2[,i]
        		r<-runif(num.subj)
        		response1[,i]<-ifelse(r<CRF1[,i],1,0)  
        		response2[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]&r>=CRF1[,i],2,0)  
        		response3[,i]<-ifelse(r>=CRF1[,i]+CRF2[,i],3,0)
        		Scale<-response1+response2+response3
        		}
        	}
        	
		if(num.resp==4){
	   		b1<-b
    		b2<-b1+.5
    		b3<-b2+.5    		
			for(i in 1:num.item){
    			CBRF1[,i]<-1/(1+exp(-1.702*a[i]*(theta-b1[i])))
        		CBRF2[,i]<-1/(1+exp(-1.702*a[i]*(theta-b2[i])))
        		CBRF3[,i]<-1/(1+exp(-1.702*a[i]*(theta-b3[i])))
        		CRF1[,i]<-1.0-CBRF1[,i]          
        		CRF2[,i]<-CBRF1[,i]-CBRF2[,i] 
        		CRF3[,i]<-CBRF2[,i]-CBRF3[,i]
        		CRF4[,i]<-CBRF3[,i]
        		r<-runif(num.subj)
        		response1[,i]<-ifelse(r<CRF1[,i],1,0)  
        		response2[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]&r>=CRF1[,i],2,0)
        		response3[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]&r>=CRF1[,i]+CRF2[,i],3,0)  
        		response4[,i]<-ifelse(r>=CRF1[,i]+CRF2[,i]+CRF3[,i],4,0)
        		Scale<-response1+response2+response3+response4
        		}
        	}
        	
	   	if(num.resp==5){	   		
	   		b1<-b
    		b2<-b1+.5
    		b3<-b2+.5
    		b4<-b3+.5
    		for(i in 1:num.item){   	
    			CBRF1[,i]<-1/(1+exp(-1.702*a[i]*(theta-b1[i])))
        		CBRF2[,i]<-1/(1+exp(-1.702*a[i]*(theta-b2[i])))
        		CBRF3[,i]<-1/(1+exp(-1.702*a[i]*(theta-b3[i])))
        		CBRF4[,i]<-1/(1+exp(-1.702*a[i]*(theta-b4[i])))
        		CRF1[,i]<-1.0-CBRF1[,i]
        		CRF2[,i]<-CBRF1[,i]-CBRF2[,i]
        		CRF3[,i]<-CBRF2[,i]-CBRF3[,i]
        		CRF4[,i]<-CBRF3[,i]-CBRF4[,i]
        		CRF5[,i]<-CBRF4[,i]
        		r<-runif(num.subj)
        		response1[,i]<-ifelse(r<CRF1[,i],1,0)  
        		response2[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]&r>=CRF1[,i],2,0)  
        		response3[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]&r>=CRF1[,i]+CRF2[,i],3,0)
        		response4[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i],4,0)
        		response5[,i]<-ifelse(r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i],5,0)
       			Scale<-response1+response2+response3+response4+response5
	   			}
    		}
        
        if(num.resp==6){	   		
	   		b1<-b
    		b2<-b1+.5
    		b3<-b2+.5
    		b4<-b3+.5
    		b5<-b4+.5
    		for(i in 1:num.item){   	
    			CBRF1[,i]<-1/(1+exp(-1.702*a[i]*(theta-b1[i])))
        		CBRF2[,i]<-1/(1+exp(-1.702*a[i]*(theta-b2[i])))
        		CBRF3[,i]<-1/(1+exp(-1.702*a[i]*(theta-b3[i])))
        		CBRF4[,i]<-1/(1+exp(-1.702*a[i]*(theta-b4[i])))
        		CBRF5[,i]<-1/(1+exp(-1.702*a[i]*(theta-b5[i])))
        		CRF1[,i]<-1.0-CBRF1[,i]
        		CRF2[,i]<-CBRF1[,i]-CBRF2[,i]
        		CRF3[,i]<-CBRF2[,i]-CBRF3[,i]
        		CRF4[,i]<-CBRF3[,i]-CBRF4[,i]
        		CRF5[,i]<-CBRF4[,i]-CBRF5[,i]
        		CRF6[,i]<-CBRF5[,i]
        		r<-runif(num.subj)
        		response1[,i]<-ifelse(r<CRF1[,i],1,0)  
        		response2[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]&r>=CRF1[,i],2,0)  
        		response3[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]&r>=CRF1[,i]+CRF2[,i],3,0)
        		response4[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i],4,0)
        		response5[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i],5,0)
        		response6[,i]<-ifelse(r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i],6,0)
       			Scale<-response1+response2+response3+response4+response5+response6
	   			}
    		}	
    			
		if(num.resp==7){	   		
	   		b1<-b
    		b2<-b1+.5
    		b3<-b2+.5
    		b4<-b3+.5
    		b5<-b4+.5
    		b6<-b5+.5
    		for(i in 1:num.item){   	
    			CBRF1[,i]<-1/(1+exp(-1.702*a[i]*(theta-b1[i])))
        		CBRF2[,i]<-1/(1+exp(-1.702*a[i]*(theta-b2[i])))
        		CBRF3[,i]<-1/(1+exp(-1.702*a[i]*(theta-b3[i])))
        		CBRF4[,i]<-1/(1+exp(-1.702*a[i]*(theta-b4[i])))
        		CBRF5[,i]<-1/(1+exp(-1.702*a[i]*(theta-b5[i])))
        		CBRF6[,i]<-1/(1+exp(-1.702*a[i]*(theta-b6[i])))
        		CRF1[,i]<-1.0-CBRF1[,i]
        		CRF2[,i]<-CBRF1[,i]-CBRF2[,i]
        		CRF3[,i]<-CBRF2[,i]-CBRF3[,i]
        		CRF4[,i]<-CBRF3[,i]-CBRF4[,i]
        		CRF5[,i]<-CBRF4[,i]-CBRF5[,i]
        		CRF6[,i]<-CBRF5[,i]-CBRF6[,i]
        		CRF7[,i]<-CBRF6[,i]
        		r<-runif(num.subj)
        		response1[,i]<-ifelse(r<CRF1[,i],1,0)  
        		response2[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]&r>=CRF1[,i],2,0)  
        		response3[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]&r>=CRF1[,i]+CRF2[,i],3,0)
        		response4[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i],4,0)
        		response5[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i],5,0)
        		response6[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i],6,0)
        		response7[,i]<-ifelse(r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i],7,0)
       			Scale<-response1+response2+response3+response4+response5+response6+response7
	   			}
    		}
    	
    	if(num.resp==8){	   		
	   		b1<-b
    		b2<-b1+.5
    		b3<-b2+.5
    		b4<-b3+.5
    		b5<-b4+.5
    		b6<-b5+.5
    		b7<-b6+.5
    		for(i in 1:num.item){   	
    			CBRF1[,i]<-1/(1+exp(-1.702*a[i]*(theta-b1[i])))
        		CBRF2[,i]<-1/(1+exp(-1.702*a[i]*(theta-b2[i])))
        		CBRF3[,i]<-1/(1+exp(-1.702*a[i]*(theta-b3[i])))
        		CBRF4[,i]<-1/(1+exp(-1.702*a[i]*(theta-b4[i])))
        		CBRF5[,i]<-1/(1+exp(-1.702*a[i]*(theta-b5[i])))
        		CBRF6[,i]<-1/(1+exp(-1.702*a[i]*(theta-b6[i])))
        		CBRF7[,i]<-1/(1+exp(-1.702*a[i]*(theta-b7[i])))
        		CRF1[,i]<-1.0-CBRF1[,i]
        		CRF2[,i]<-CBRF1[,i]-CBRF2[,i]
        		CRF3[,i]<-CBRF2[,i]-CBRF3[,i]
        		CRF4[,i]<-CBRF3[,i]-CBRF4[,i]
        		CRF5[,i]<-CBRF4[,i]-CBRF5[,i]
        		CRF6[,i]<-CBRF5[,i]-CBRF6[,i]
        		CRF7[,i]<-CBRF6[,i]-CBRF7[,i]
        		CRF8[,i]<-CBRF7[,i]
        		r<-runif(num.subj)
        		response1[,i]<-ifelse(r<CRF1[,i],1,0)  
        		response2[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]&r>=CRF1[,i],2,0)  
        		response3[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]&r>=CRF1[,i]+CRF2[,i],3,0)
        		response4[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i],4,0)
        		response5[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i],5,0)
        		response6[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i],6,0)
        		response7[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i]+CRF7[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i],7,0)
        		response8[,i]<-ifelse(r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i]+CRF7[,i],8,0)
       			Scale<-response1+response2+response3+response4+response5+response6+response7+response8
	   			}
    		}
    	
    	if(num.resp==9){	   		
	   		b1<-b
    		b2<-b1+.5
    		b3<-b2+.5
    		b4<-b3+.5
    		b5<-b4+.5
    		b6<-b5+.5
    		b7<-b6+.5
    		b8<-b7+.5
    		for(i in 1:num.item){   	
    			CBRF1[,i]<-1/(1+exp(-1.702*a[i]*(theta-b1[i])))
        		CBRF2[,i]<-1/(1+exp(-1.702*a[i]*(theta-b2[i])))
        		CBRF3[,i]<-1/(1+exp(-1.702*a[i]*(theta-b3[i])))
        		CBRF4[,i]<-1/(1+exp(-1.702*a[i]*(theta-b4[i])))
        		CBRF5[,i]<-1/(1+exp(-1.702*a[i]*(theta-b5[i])))
        		CBRF6[,i]<-1/(1+exp(-1.702*a[i]*(theta-b6[i])))
        		CBRF7[,i]<-1/(1+exp(-1.702*a[i]*(theta-b7[i])))
        		CBRF8[,i]<-1/(1+exp(-1.702*a[i]*(theta-b8[i])))
        		CRF1[,i]<-1.0-CBRF1[,i]
        		CRF2[,i]<-CBRF1[,i]-CBRF2[,i]
        		CRF3[,i]<-CBRF2[,i]-CBRF3[,i]
        		CRF4[,i]<-CBRF3[,i]-CBRF4[,i]
        		CRF5[,i]<-CBRF4[,i]-CBRF5[,i]
        		CRF6[,i]<-CBRF5[,i]-CBRF6[,i]
        		CRF7[,i]<-CBRF6[,i]-CBRF7[,i]
        		CRF8[,i]<-CBRF7[,i]-CBRF8[,i]
        		CRF9[,i]<-CBRF8[,i]
        		r<-runif(num.subj)
        		response1[,i]<-ifelse(r<CRF1[,i],1,0)  
        		response2[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]&r>=CRF1[,i],2,0)  
        		response3[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]&r>=CRF1[,i]+CRF2[,i],3,0)
        		response4[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i],4,0)
        		response5[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i],5,0)
        		response6[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i],6,0)
        		response7[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i]+CRF7[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i],7,0)
        		response8[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i]+CRF7[,i]+CRF8[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i]+CRF7[,i],8,0)
        		response9[,i]<-ifelse(r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i]+CRF7[,i]+CRF8[,i],9,0)
       			Scale<-response1+response2+response3+response4+response5+response6+response7+response8+response9
	   			}
    		}
    	
    	if(num.resp==10){	   		
	   		b1<-b
    		b2<-b1+.5
    		b3<-b2+.5
    		b4<-b3+.5
    		b5<-b4+.5
    		b6<-b5+.5
    		b7<-b6+.5
    		b8<-b7+.5
    		b9<-b8+.5
    		for(i in 1:num.item){   	
    			CBRF1[,i]<-1/(1+exp(-1.702*a[i]*(theta-b1[i])))
        		CBRF2[,i]<-1/(1+exp(-1.702*a[i]*(theta-b2[i])))
        		CBRF3[,i]<-1/(1+exp(-1.702*a[i]*(theta-b3[i])))
        		CBRF4[,i]<-1/(1+exp(-1.702*a[i]*(theta-b4[i])))
        		CBRF5[,i]<-1/(1+exp(-1.702*a[i]*(theta-b5[i])))
        		CBRF6[,i]<-1/(1+exp(-1.702*a[i]*(theta-b6[i])))
        		CBRF7[,i]<-1/(1+exp(-1.702*a[i]*(theta-b7[i])))
        		CBRF8[,i]<-1/(1+exp(-1.702*a[i]*(theta-b8[i])))
        		CBRF9[,i]<-1/(1+exp(-1.702*a[i]*(theta-b9[i])))
        		CRF1[,i]<-1.0-CBRF1[,i]
        		CRF2[,i]<-CBRF1[,i]-CBRF2[,i]
        		CRF3[,i]<-CBRF2[,i]-CBRF3[,i]
        		CRF4[,i]<-CBRF3[,i]-CBRF4[,i]
        		CRF5[,i]<-CBRF4[,i]-CBRF5[,i]
        		CRF6[,i]<-CBRF5[,i]-CBRF6[,i]
        		CRF7[,i]<-CBRF6[,i]-CBRF7[,i]
        		CRF8[,i]<-CBRF7[,i]-CBRF8[,i]
        		CRF9[,i]<-CBRF8[,i]-CBRF9[,i]
        		CRF10[,i]<-CBRF9[,i]
        		r<-runif(num.subj)
        		response1[,i]<-ifelse(r<CRF1[,i],1,0)  
        		response2[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]&r>=CRF1[,i],2,0)  
        		response3[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]&r>=CRF1[,i]+CRF2[,i],3,0)
        		response4[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i],4,0)
        		response5[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i],5,0)
        		response6[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i],6,0)
        		response7[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i]+CRF7[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i],7,0)
        		response8[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i]+CRF7[,i]+CRF8[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i]+CRF7[,i],8,0)
        		response9[,i]<-ifelse(r<CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i]+CRF7[,i]+CRF8[,i]+CRF9[,i]&r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i]+CRF7[,i]+CRF8[,i],9,0)
        		response10[,i]<-ifelse(r>=CRF1[,i]+CRF2[,i]+CRF3[,i]+CRF4[,i]+CRF5[,i]+CRF6[,i]+CRF7[,i]+CRF8[,i]+CRF9[,i],10,0)
       			Scale<-response1+response2+response3+response4+response5+response6+response7+response8+response9+response10
	   			}
    		}
		
		alpha<-round((num.item/(num.item-1))*(1-sum(diag(var(Scale)))/sum(var(Scale))),3)
        write.table(Scale,file="MorseGen Survey Results.txt",row.names=FALSE)
        list(SCORES=Scale, RELIABILITY=alpha)
        }
        else
        cat("Number of Response Options Out of Range (possible values = 3, 4, 5, 6, 7, 8, 9, or 10)")
        }