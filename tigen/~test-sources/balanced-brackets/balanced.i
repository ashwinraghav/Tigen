
int isBal(char*s, int l){
    int c=0;
    while(l--) {
	if(s[l]==']') ++c;
	else if(s[l]=='[') if(--c<0) break;
    } 
    return !c;
}
