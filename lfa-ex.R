histogram(~ firstpumparriving_attendancetime | incgeo_boroughname,
          scales=list(y=list(relation="free"),x=list(relation="free")),
            panel = function(x, ...) {
              panel.histogram(x=x,...)
              m<-round(mean(x,na.rm=TRUE))
              d<-m-360
              panel.abline(v=m,col='red')
              panel.abline(v=360,col='black')
              panel.text(x=900,y=20,seconds_to_period(m))
              panel.text(x=900,y=17,d)
              }
            )
