struct foo
{
        float f1,f2;
};
int result (float a, float b)
{
        struct foo i1,i2;
        i1.f1 = 1.7;
        i1.f2 = 2.5;
        i2.f1 = 2.0;
        i2.f2 = 2.5;
        if (a > i1.f1){
                if (a < i1.f2){
                        if (a < i2.f1){
                                return 0;
                        }else
                        {
                                return 1;
                        }
                        
                }else {
                        return 1;
                }
        }else{
                if (b < i1.f2){
                        if (b < i2.f1){
                                return 0;
                        }else
                        {
                                return 1;
                        }
                        
                }else {
                        return 1;
                }
                
        }
        return 1;
}
