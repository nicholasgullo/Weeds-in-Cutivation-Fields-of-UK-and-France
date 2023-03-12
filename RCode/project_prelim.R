data <- read.csv(file = 'data.csv')

cd_ref = data['taxref10.CD_REF']
name = data['binome.discoweed']
family = data['family']
biogeo = data['biogeo']
coeff_participation = data['c']
taxon_freq = data['z']


red_fr = data['red.FR']
red_uk = data['red.UK']

uniq_fam = unique(family)
nrow(uniq_fam)

#Family to redlist status
#do some families have more red list statuses

new_data = data.frame(family, red_fr, red_uk, coeff_participation, taxon_freq)

# Change NA to NE
new_data$red.FR[is.na(new_data$red.FR)] = "NE"
new_data$red.UK[is.na(new_data$red.UK)] = "NE"

# Removal all where coeff part is NA
new_data = subset(new_data, new_data$c !="NA")

# Create numbers and mappings for red lists
redlist_names = c('NE', 'DD', 'LC', 'NT', 'VU', 'EN', 'CR', 'EW', 'EX')
redlist_nums =  c(1, 2, 3, 4, 5, 6, 7, 8, 9)

redlist_key = data.frame(redlist_names, redlist_nums)

# Convert red.FR
for(i in 1:nrow(new_data['red.FR'])) {
  new_num = 0
  for(j in 1:9){
    if(new_data$red.FR[i] == redlist_names[j]){
      new_num = j
      break
    }
  }
  new_data$red.FR[i] = new_num;
}

# Convert red.UK
for(i in 1:nrow(new_data['red.UK'])) {
  new_num = 0
  for(j in 1:9){
    if(new_data$red.UK[i] == redlist_names[j]){
      new_num = j
      break
    }
  }
  new_data$red.UK[i] = new_num;
}
#print out new data set
new_data

#make a list of all the family names
family_names = unique(new_data$family)

family_nums = rep(0,73)
threat_nums = rep(0,73)
coeff_participation = rep(0,73)
freq = rep(0,73)

for(i in 1:nrow(new_data['family'])) {
  
  for(j in 1:73){
    if(new_data$family[i] == family_names[j]){ 
      family_nums[j] = family_nums[j]+1; #coutnting number of species in each family
      coeff_participation[j] = coeff_participation[j] + new_data$c[i]
      freq[j] = freq[j] + new_data$z[i]
      #these two if statements sum are what count the number of threatened species in a family and store that as threat_nums
      if(new_data$red.FR[i]>4){
        threat_nums[j] = threat_nums[j] +1;
      }
      else{
        if(new_data$red.UK[i]>4){
          threat_nums[j] = threat_nums[j] +1;
        }
      }
      break
    }
  }
}


percent_threat = (threat_nums/family_nums)*100
par(mar=c(11,4,4,4))
xx = barplot(percent_threat, names=family_names, las=2, cex.names=0.7, ylab="Percent Threat", main="Percent Threat of Weed Families", ylim = c(0,50))
text(x = xx + 0.4, y = percent_threat + 1, las=2, label = round(percent_threat,3), pos = 3, cex = 0.7, col = "black", srt=90)
text(xx[length(xx)/2], -12, label = "Family", pos = 4, xpd=NA)

# Graph the coef of participation
percent_coeff_participation = (coeff_participation/family_nums)*100
barplot(percent_coeff_participation, names=family_names, las=2, cex.names=0.7, ylab="Coefficient of Participation", main="Coefficient of Participation of Weed Families", ylim=c(0,80))#changed range(pretty(c(0, coeff_participation)))
text(xx[length(xx)/2], -12, label = "Family", pos = 4, xpd=NA)

# correlation (there is none)
cor(percent_threat, percent_coeff_participation)


# second question
percent_freq = (freq/family_nums)*100
cor(percent_freq, percent_coeff_participation)
plot(percent_freq, percent_coeff_participation)