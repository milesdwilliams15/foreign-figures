# Clean Data ----

## Packages ----
library(tidyverse)
library(peacesciencer)

## Data ----

### The raw MIE dataset ----
mie <- read_csv("004_int_ord/_data/mie-1.0.csv")

### Expand data so nothing is missing ----
mie_rev <- mie |>
  mutate(
    ccode2 = mie$ccode1,
    ccode1 = mie$ccode2,
    fatalmin1 = mie$fatalmin2,
    fatalmin2 = mie$fatalmin1,
    fatalmax1 = mie$fatalmax2,
    fatalmax2 = mie$fatalmin1
  )

### Convert to undirected dyads ----
mie_full <- bind_rows(mie, mie_rev) |>
  filter(ccode1 < ccode2)

### Aggregate MIEs to years ----
mie_full |>
  filter(hostlev > 3) |>
  group_by(ccode1, ccode2, styear) |>
  summarize(
    mie_count = n(),
    across(starts_with("fatal"), sum)
  ) |>
  ungroup() |>
  rename(year = styear) -> mie_years

### An undirected dyad dataset ----
dd <- create_dyadyears(
  subset_years = 1816:2014,
  directed = F
)

### Merge MIE with all undirected dyads ----
mydata <- left_join(dd, mie_years, by = c("ccode1", "ccode2", "year"))

### Braumoeller Int'l Order code ----
# Use code from:
# https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/Q90FOB/KBGKHK&version=1.0
# This was code used by Braumoeller to code Int'l Orders
#
# Add codings from Andy Goodhart for 19th and 20th century international orders
# Very generous definition of international order to avoid missing any
# Also codes new states that arose from the disintegration of order

# Format of variable names is c1.xxx and c2.xxx for c1, c2. xxx's are:
# LatinAm: Latin American Decolonization from 1810-1858
# concert: Concert of Europe from 1816-1852
# concert.gp: Concert of Europe, Great Powers only
# interim: period btw Concert and Bismarck
# bismarck: Bismarckian order
# IndependentAfterWWI: Newly independent states after WWI
# MandateAfterWWI: Middle East territories under int'l mandate (coded by countries that emerged)
# League: League of Nations
# PostWarLiberal: Post-WWII liberal international order
# PostWarCommunist: Post-WWII communist international order
# PostWarOther: Third World and nonaligned middle powers

# Latin American Decolonization from 1810-1858. Note that 1858 is 30 years after the last declaration of independence in this period (i.e., Uruguay in 1828).

mydata$c1.LatinAm<-0
mydata$c1.LatinAm[mydata$ccode1==160 & mydata$year>1815 & mydata$year<1857]<- 1 # Argentina declared independence in 1816.
mydata$c1.LatinAm[mydata$ccode1==145 & mydata$year>1824 & mydata$year<1857]<- 1 # Bolivia gained indepdendence in 1825.
mydata$c1.LatinAm[mydata$ccode1==140 & mydata$year>1821 & mydata$year<1857]<- 1 # Brazil gained independence in 1822.
mydata$c1.LatinAm[mydata$ccode1==155 & mydata$year>1809 & mydata$year<1857]<- 1 # Chile broke from Spanish rule in 1810 and the royalist forces collapsed in 1826.
mydata$c1.LatinAm[mydata$ccode1==100 & mydata$year>1819 & mydata$year<1857]<- 1 # Colombia gained independence from the Spanish in 1820.
mydata$c1.LatinAm[mydata$ccode1==94 & mydata$year>1820 & mydata$year<1857]<- 1 # Costa Rica gained independence in 1821.
mydata$c1.LatinAm[mydata$ccode1==130 & mydata$year>1821 & mydata$year<1857]<- 1 # Ecuador gained independence in 1822.
mydata$c1.LatinAm[mydata$ccode1==92 & mydata$year>1820 & mydata$year<1857]<- 1 # El Salvador gained independence in 1821.
mydata$c1.LatinAm[mydata$ccode1==90 & mydata$year>1820 & mydata$year<1857]<- 1 # Guatemala gained independence in 1821.
mydata$c1.LatinAm[mydata$ccode1==91 & mydata$year>1820 & mydata$year<1857]<- 1 # Honduras gained independence in 1821.
mydata$c1.LatinAm[mydata$ccode1==70 & mydata$year>1820 & mydata$year<1857]<- 1 # Mexico gained independence in 1821.
mydata$c1.LatinAm[mydata$ccode1==93 & mydata$year>1820 & mydata$year<1857]<- 1 # Nicaragua gained independence in 1821 but control of the country was not yet consolidated.
mydata$c1.LatinAm[mydata$ccode1==95 & mydata$year>1820 & mydata$year<1857]<- 1 # Panama gained independence in 1821.
mydata$c1.LatinAm[mydata$ccode1==135 & mydata$year>1820 & mydata$year<1857]<- 1 # Peru declared independence in 1821 and defeated the Spanish in 1824.
mydata$c1.LatinAm[mydata$ccode1==150 & mydata$year>1810 & mydata$year<1857]<- 1 # Paraguay gained independence in 1811.
mydata$c1.LatinAm[mydata$ccode1==165 & mydata$year>1827 & mydata$year<1857]<- 1 # Uruguay gained independence in 1828.
mydata$c1.LatinAm[mydata$ccode1==101 & mydata$year>1820 & mydata$year<1857]<- 1 # Venezuela gained independence in 1821.

mydata$c2.LatinAm<-0
mydata$c2.LatinAm[mydata$ccode2==160 & mydata$year>1815 & mydata$year<1857]<- 1 # Argentina declared independence in 1816.
mydata$c2.LatinAm[mydata$ccode2==145 & mydata$year>1824 & mydata$year<1857]<- 1 # Bolivia gained indepdendence in 1825.
mydata$c2.LatinAm[mydata$ccode2==140 & mydata$year>1821 & mydata$year<1857]<- 1 # Brazil gained independence in 1822.
mydata$c2.LatinAm[mydata$ccode2==155 & mydata$year>1809 & mydata$year<1857]<- 1 # Chile broke from Spanish rule in 1810 and the royalist forces collapsed in 1826.
mydata$c2.LatinAm[mydata$ccode2==100 & mydata$year>1819 & mydata$year<1857]<- 1 # Colombia gained independence from the Spanish in 1820.
mydata$c2.LatinAm[mydata$ccode2==94 & mydata$year>1820 & mydata$year<1857]<- 1 # Costa Rica gained independence in 1821.
mydata$c2.LatinAm[mydata$ccode2==130 & mydata$year>1821 & mydata$year<1857]<- 1 # Ecuador gained independence in 1822.
mydata$c2.LatinAm[mydata$ccode2==92 & mydata$year>1820 & mydata$year<1857]<- 1 # El Salvador gained independence in 1821.
mydata$c2.LatinAm[mydata$ccode2==90 & mydata$year>1820 & mydata$year<1857]<- 1 # Guatemala gained independence in 1821.
mydata$c2.LatinAm[mydata$ccode2==91 & mydata$year>1820 & mydata$year<1857]<- 1 # Honduras gained independence in 1821.
mydata$c2.LatinAm[mydata$ccode2==70 & mydata$year>1820 & mydata$year<1857]<- 1 # Mexico gained independence in 1821.
mydata$c2.LatinAm[mydata$ccode2==93 & mydata$year>1820 & mydata$year<1857]<- 1 # Nicaragua gained independence in 1821 but control of the country was not yet consolidated.
mydata$c2.LatinAm[mydata$ccode2==95 & mydata$year>1820 & mydata$year<1857]<- 1 # Panama gained independence in 1821.
mydata$c2.LatinAm[mydata$ccode2==135 & mydata$year>1820 & mydata$year<1857]<- 1 # Peru declared independence in 1821 and defeated the Spanish in 1824.
mydata$c2.LatinAm[mydata$ccode2==150 & mydata$year>1810 & mydata$year<1857]<- 1 # Paraguay gained independence in 1811.
mydata$c2.LatinAm[mydata$ccode2==165 & mydata$year>1827 & mydata$year<1857]<- 1 # Uruguay gained independence in 1828.
mydata$c2.LatinAm[mydata$ccode2==101 & mydata$year>1820 & mydata$year<1857]<- 1 # Venezuela gained independence in 1821.


# Concert of Europe from 1816-1852: Includes all states in Europe (CCODES 199-399)

mydata$c1.concert<-0
mydata$c1.concert[mydata$ccode1>199 & mydata$ccode1<400 & mydata$year>1815 & mydata$year<1853]<- 1

mydata$c2.concert<- 0
mydata$c2.concert[mydata$ccode2>199 & mydata$ccode2<400 & mydata$year>1815 & mydata$year<1853]<- 1

mydata$c1.concert.gp<-0
mydata$c1.concert.gp[mydata$ccode1 %in% c(200,220,255,300,365) & mydata$year>1815 & mydata$year<1853]<- 1

mydata$c2.concert.gp<- 0
mydata$c2.concert.gp[mydata$ccode2 %in% c(200,220,255,300,365) & mydata$year>1815 & mydata$year<1853]<- 1


# Bismarckian Order from 1855-1870: Includes all states in Europe (CCODES 199-399)

mydata$c1.interim<-0
mydata$c1.interim[mydata$ccode1>199 & mydata$ccode1<400 & mydata$year>1854 & mydata$year<1871]<- 1

mydata$c2.interim<- 0
mydata$c2.interim[mydata$ccode2>199 & mydata$ccode2<400 & mydata$year>1854 & mydata$year<1871]<- 1

# Bismarckian Order from 1871-1890: Includes all states in Europe (CCODES 199-399)

mydata$c1.bismarck<-0
mydata$c1.bismarck[mydata$ccode1>199 & mydata$ccode1<400 & mydata$year>1870 & mydata$year<1891]<- 1

mydata$c2.bismarck<- 0
mydata$c2.bismarck[mydata$ccode2>199 & mydata$ccode2<400 & mydata$year>1870 & mydata$year<1891]<- 1



# Wilhelmine System from 1890-1913

mydata$c1.wilhelm<-0
mydata$c1.wilhelm[mydata$ccode1>199 & mydata$ccode1<400 & mydata$year>1889 & mydata$year<1915]<- 1

mydata$c2.wilhelm<- 0
mydata$c2.wilhelm[mydata$ccode2>199 & mydata$ccode2<400 & mydata$year>1889 & mydata$year<1915]<- 1

# Newly Independent States after the First World War (Source: https://en.wikipedia.org/wiki/Aftermath_of_World_War_I#New_nations_break_free; https://www.britannica.com/place/Yugoslavia-former-federated-nation-1929-2003;
# https://www.loc.gov/law/help/us-treaties/bevans/m-ust000002-0043.pdf; and various pages in the CIA World Factbook and Encyclopedia Britannica online; https://history.state.gov/countries/egypt

mydata$c1.IndependentAfterWWI<-0
mydata$c1.IndependentAfterWWI[mydata$ccode1==700 & mydata$year>1918 & mydata$year<1939]<-1 # Afghanistan gained independence from Britain in 1919.
mydata$c1.IndependentAfterWWI[mydata$ccode1==339 & mydata$year>1911 & mydata$year<1939]<-1 # Albania gained independence from the Ottomans in 1912.
mydata$c1.IndependentAfterWWI[mydata$ccode1==371 & mydata$year>1917 & mydata$year<1921]<-1 # Armenia was briefly independent from 1918 to 1920.
mydata$c1.IndependentAfterWWI[mydata$ccode1==305 & mydata$year>1917 & mydata$year<1939]<-1 # Austria emerged as an independent country in 1918 and remained so until the German Anschluss of 1938.
mydata$c1.IndependentAfterWWI[mydata$ccode1==373 & mydata$year>1917 & mydata$year<1921]<-1 # Azerbaijan was briefly independent from 1918-1920 but had imperfect control over its territory. The Russians invaded in 1920.
mydata$c1.IndependentAfterWWI[mydata$ccode1==355 & mydata$year>1907 & mydata$year<1939]<-1 # Bulgaria gained independence from the Ottomans in 1908.
mydata$c1.IndependentAfterWWI[mydata$ccode1==315 & mydata$year>1917 & mydata$year<1940]<-1 # Czechoslovakia was established in 1918. Germany annexed part of it in 1938.
mydata$c1.IndependentAfterWWI[mydata$ccode1==651 & mydata$year>1921 & mydata$year<1940]<-1 # Egypt gained indendepence in 1922.
mydata$c1.IndependentAfterWWI[mydata$ccode1==366 & mydata$year>1917 & mydata$year<1940]<-1 # Estonia declared indendepence in 1918. 
mydata$c1.IndependentAfterWWI[mydata$ccode1==375 & mydata$year>1916 & mydata$year<1940]<-1 # Finland gained independence in 1917.
mydata$c1.IndependentAfterWWI[mydata$ccode1==372 & mydata$year>1917 & mydata$year<1922]<-1 # Georgia was briefly independent from 1918-1921. 
mydata$c1.IndependentAfterWWI[mydata$ccode1==310 & mydata$year>1917 & mydata$year<1940]<-1 # Hungary emerged as an independent country in 1918.
mydata$c1.IndependentAfterWWI[mydata$ccode1==205 & mydata$year>1920 & mydata$year<1940]<-1 # Ireland gained independence from Britain in 1921. 
mydata$c1.IndependentAfterWWI[mydata$ccode1==367 & mydata$year>1917 & mydata$year<1940]<-1 # Latvia gained independence in 1918. 
mydata$c1.IndependentAfterWWI[mydata$ccode1==368 & mydata$year>1917 & mydata$year<1940]<-1 # Lithuania gained independence in 1918.
mydata$c1.IndependentAfterWWI[mydata$ccode1==290 & mydata$year>1917 & mydata$year<1940]<-1 # Poland gained independence in 1918.
mydata$c1.IndependentAfterWWI[mydata$ccode1==670 & mydata$year>1931 & mydata$year<1939]<-1 # Saudi Arabia became independent and unified in 1932.
mydata$c1.IndependentAfterWWI[mydata$ccode1==369 & mydata$year>1917 & mydata$year<1921]<-1 # Ukraine was briefly independent from 1918-1920. 
mydata$c1.IndependentAfterWWI[mydata$ccode1==678 & mydata$year>1917 & mydata$year<1939]<-1 # Yemen (Yemen Arab Republic/North Yemen) gained independence from the Ottomans in 1918.
mydata$c1.IndependentAfterWWI[mydata$ccode1==345 & mydata$year>1918 & mydata$year<1940]<-1 # Yugoslavia (known as Kingdom of Serbs, Croats, and Slovenes est. by Paris Peace Conf. in 1919 and became Yugoslavia in 1929)

mydata$c2.IndependentAfterWWI<-0
mydata$c2.IndependentAfterWWI[mydata$ccode2==700 & mydata$year>1918 & mydata$year<1939]<-1 # Afghanistan gained independence from Britain in 1919.
mydata$c2.IndependentAfterWWI[mydata$ccode2==339 & mydata$year>1911 & mydata$year<1939]<-1 # Albania gained independence from the Ottomans in 1912.
mydata$c2.IndependentAfterWWI[mydata$ccode2==371 & mydata$year>1917 & mydata$year<1921]<-1 # Armenia was briefly independent from 1918 to 1920.
mydata$c2.IndependentAfterWWI[mydata$ccode2==305 & mydata$year>1917 & mydata$year<1939]<-1 # Austria emerged as an independent country in 1918 and remained so until the German Anschluss of 1938.
mydata$c2.IndependentAfterWWI[mydata$ccode2==373 & mydata$year>1917 & mydata$year<1921]<-1 # Azerbaijan was briefly independent from 1918-1920 but had imperfect control over its territory. The Russians invaded in 1920.
mydata$c2.IndependentAfterWWI[mydata$ccode2==355 & mydata$year>1907 & mydata$year<1939]<-1 # Bulgaria gained independence from the Ottomans in 1908.
mydata$c2.IndependentAfterWWI[mydata$ccode2==315 & mydata$year>1917 & mydata$year<1940]<-1 # Czechoslovakia was established in 1918. Germany annexed part of it in 1938.
mydata$c1.IndependentAfterWWI[mydata$ccode2==651 & mydata$year>1921 & mydata$year<1940]<-1 # Egypt gained indendepence in 1922.
mydata$c2.IndependentAfterWWI[mydata$ccode2==366 & mydata$year>1917 & mydata$year<1940]<-1 # Estonia declared indendepence in 1918. 
mydata$c2.IndependentAfterWWI[mydata$ccode2==375 & mydata$year>1916 & mydata$year<1940]<-1 # Finland gained independence in 1917.
mydata$c2.IndependentAfterWWI[mydata$ccode2==372 & mydata$year>1917 & mydata$year<1922]<-1 # Georgia was briefly independent from 1918-1921. 
mydata$c2.IndependentAfterWWI[mydata$ccode2==310 & mydata$year>1917 & mydata$year<1940]<-1 # Hungary emerged as an independent country in 1918.
mydata$c2.IndependentAfterWWI[mydata$ccode2==205 & mydata$year>1920 & mydata$year<1940]<-1 # Ireland gained independence from Britain in 1921. 
mydata$c2.IndependentAfterWWI[mydata$ccode2==367 & mydata$year>1917 & mydata$year<1940]<-1 # Latvia gained independence in 1918. 
mydata$c2.IndependentAfterWWI[mydata$ccode2==368 & mydata$year>1917 & mydata$year<1940]<-1 # Lithuania gained independence in 1918.
mydata$c2.IndependentAfterWWI[mydata$ccode2==290 & mydata$year>1917 & mydata$year<1940]<-1 # Poland gained independence in 1918.
mydata$c2.IndependentAfterWWI[mydata$ccode2==670 & mydata$year>1931 & mydata$year<1939]<-1 # Saudi Arabia became independent and unified in 1932.
mydata$c2.IndependentAfterWWI[mydata$ccode2==369 & mydata$year>1917 & mydata$year<1921]<-1 # Ukraine was briefly independent from 1918-1920. 
mydata$c2.IndependentAfterWWI[mydata$ccode2==678 & mydata$year>1917 & mydata$year<1939]<-1 # Yemen (Yemen Arab Republic/North Yemen) gained independence from the Ottomans in 1918.
mydata$c2.IndependentAfterWWI[mydata$ccode2==345 & mydata$year>1918 & mydata$year<1940]<-1 # Yugoslavia (known as Kingdom of Serbs, Croats, and Slovenes est. by Paris Peace Conf. in 1919 and became Yugoslavia in 1929)

# Middle East territories under International Mandate, post-World War I (coded by the countries that emerged from those territories).  
# Note that this section does not include Middle East territories under British control/influence (but not formal mandate) during this period. 
# (UAE, Bahrain, Qatar, Kuwait had relationships with Britain that predate WWI.)
# Present day Palestine (West Bank & Gaza) does not have a CCODE so it is absent here. 
# Sourcs: https://www.britannica.com/place/Syria/The-French-mandate; https://history.state.gov/countries/syria; https://history.state.gov/countries/lebanon; 
# https://en.wikipedia.org/wiki/Anglo-Iraqi_Treaty; https://en.wikipedia.org/wiki/British_Mandate_for_Mesopotamia_(legal_instrument);

mydata$c1.MandateAfterWWI<-0
mydata$c1.MandateAfterWWI[mydata$ccode1==652 & mydata$year>1921 & mydata$year<1940]<-1 # Syria: French Mandate started in 1922
mydata$c1.MandateAfterWWI[mydata$ccode1==660 & mydata$year>1921 & mydata$year<1940]<-1 # Lebanon: French Mandate started in 1922
mydata$c1.MandateAfterWWI[mydata$ccode1==645 & mydata$year>1921 & mydata$year<1940]<-1 # Iraq: British Mandate for Mesopotamia enacted 1922
mydata$c1.MandateAfterWWI[mydata$ccode1==666 & mydata$year>1921 & mydata$year<1940]<-1 # Israel: British Mandate for Palestine enacted 1922
mydata$c1.MandateAfterWWI[mydata$ccode1==663 & mydata$year>1921 & mydata$year<1940]<-1 # Jordan: British Mandate for Palestine enacted 1922

mydata$c2.MandateAfterWWI<-0
mydata$c2.MandateAfterWWI[mydata$ccode2==652 & mydata$year>1921 & mydata$year<1940]<-1 # Syria: French Mandate started in 1922
mydata$c2.MandateAfterWWI[mydata$ccode2==660 & mydata$year>1921 & mydata$year<1940]<-1 # Lebanon: French Mandate started in 1922
mydata$c2.MandateAfterWWI[mydata$ccode2==645 & mydata$year>1921 & mydata$year<1940]<-1 # Iraq: British Mandate for Mesopotamia enacted 1922
mydata$c2.MandateAfterWWI[mydata$ccode2==666 & mydata$year>1921 & mydata$year<1940]<-1 # Israel: British Mandate for Palestine enacted 1922
mydata$c2.MandateAfterWWI[mydata$ccode2==663 & mydata$year>1921 & mydata$year<1940]<-1 # Jordan: British Mandate for Palestine enacted 1922

# Leage of Nations System from 1919-1939 (Source: http://www.indiana.edu/~league/nationalmember.htm, accessed 9 Feb 2017)

mydata$c1.League<-0
mydata$c1.League[mydata$ccode1==700 & mydata$year>1933 & mydata$year<1940]<- 1 # Afghanistan 1934-1939
mydata$c1.League[mydata$ccode1==339 & mydata$year>1919 & mydata$year<1940]<- 1 # Albania 1920-1939(annexed by Italy April, 12 1939)
mydata$c1.League[mydata$ccode1==160 & mydata$year>1919 & mydata$year<1940]<- 1 # Argentina 1920-1939
mydata$c1.League[mydata$ccode1==900 & mydata$year>1919 & mydata$year<1940]<- 1 # Australia 1920-1939
mydata$c1.League[mydata$ccode1==305 & mydata$year>1919 & mydata$year<1939]<- 1 # Austria 1920-1939 (annexed by Germany April 10, 1938)
mydata$c1.League[mydata$ccode1==211 & mydata$year>1919 & mydata$year<1940]<- 1 # Belgium 1920-1939
mydata$c1.League[mydata$ccode1==145 & mydata$year>1919 & mydata$year<1940]<- 1 # Bolivia 1920-1939
mydata$c1.League[mydata$ccode1==140 & mydata$year>1919 & mydata$year<1927]<- 1 # Brazil 1920-1926
mydata$c1.League[mydata$ccode1==200 & mydata$year>1919 & mydata$year<1940]<- 1 # United Kingdom 1920-1939 # NOTE THAT BRITISH EMPIRE WAS THE SAME -- NEED TO ADD THEM. 
mydata$c1.League[mydata$ccode1==355 & mydata$year>1919 & mydata$year<1940]<- 1 # Bulgaria 1920-1939
mydata$c1.League[mydata$ccode1==20 & mydata$year>1919 & mydata$year<1940]<- 1 # Canada 1920-1939
mydata$c1.League[mydata$ccode1==155 & mydata$year>1919 & mydata$year<1939]<- 1 # Chile 1920-1938
mydata$c1.League[mydata$ccode1==710 & mydata$year>1919 & mydata$year<1940]<- 1 # China 1920-1939
mydata$c1.League[mydata$ccode1==100 & mydata$year>1919 & mydata$year<1940]<- 1 # Colombia 1920-1939
mydata$c1.League[mydata$ccode1==94 & mydata$year>1919 & mydata$year<1926]<- 1 # Costa Rica 1920-1925
mydata$c1.League[mydata$ccode1==40 & mydata$year>1919 & mydata$year<1940]<- 1 # Cuba 1920-1939
mydata$c1.League[mydata$ccode1==315 & mydata$year>1919 & mydata$year<1940]<- 1 # Czechoslovakia 1920-1939 (annexed by Germany March 15, 1939)
mydata$c1.League[mydata$ccode1==390 & mydata$year>1919 & mydata$year<1940]<- 1 # Denmark 1920-1939
mydata$c1.League[mydata$ccode1==42 & mydata$year>1923 & mydata$year<1940]<- 1 # Dominican Republic 1924-1939
mydata$c1.League[mydata$ccode1==130 & mydata$year>1933 & mydata$year<1940]<- 1 # Ecuador 1934-1939
mydata$c1.League[mydata$ccode1==651 & mydata$year>1936 & mydata$year<1940]<- 1 # Egypt 1937-1939
mydata$c1.League[mydata$ccode1==366 & mydata$year>1920 & mydata$year<1940]<- 1 # Estonia 1921-1939
mydata$c1.League[mydata$ccode1==530 & mydata$year>1922 & mydata$year<1937]<- 1 # Ethiopia 1923-1936 (annexed by Italy May 9, 1936)
mydata$c1.League[mydata$ccode1==375 & mydata$year>1919 & mydata$year<1940]<- 1 # Finland 1920-1939
mydata$c1.League[mydata$ccode1==220 & mydata$year>1919 & mydata$year<1942]<- 1 # France 1920-1941 ### WHY WAS THIS LATER? DID THEY JUST NOT FORMALLY WITHDRAW UNTIL THEN?
mydata$c1.League[mydata$ccode1==255 & mydata$year>1925 & mydata$year<1934]<- 1 # Germany 1926-1933
mydata$c1.League[mydata$ccode1==350 & mydata$year>1919 & mydata$year<1940]<- 1 # Greece 1920-1939
mydata$c1.League[mydata$ccode1==90 & mydata$year>1919 & mydata$year<1937]<- 1 # Guatemala 1920-1936
mydata$c1.League[mydata$ccode1==41 & mydata$year>1919 & mydata$year<1940]<- 1 # Haiti 1920-1942 ### CHECK THIS END DATE
mydata$c1.League[mydata$ccode1==91 & mydata$year>1919 & mydata$year<1937]<- 1 # Honduras 1920-1936
mydata$c1.League[mydata$ccode1==310 & mydata$year>1921 & mydata$year<1940]<- 1 # Hungary 1922-1939
mydata$c1.League[mydata$ccode1==750 & mydata$year>1919 & mydata$year<1940]<- 1 # India 1920-1939
mydata$c1.League[mydata$ccode1==630 & mydata$year>1919 & mydata$year<1940]<- 1 # Iran 1920-1939
mydata$c1.League[mydata$ccode1==645 & mydata$year>1931 & mydata$year<1940]<- 1 # Iraq 1932-1939
mydata$c1.League[mydata$ccode1==205 & mydata$year>1920 & mydata$year<1940]<- 1 # Ireland gained independence from Britain in 1921.
mydata$c1.League[mydata$ccode1==325 & mydata$year>1919 & mydata$year<1938]<- 1 # Italy 1920-1937
mydata$c1.League[mydata$ccode1==740 & mydata$year>1919 & mydata$year<1934]<- 1 # Japan 1920-1933
mydata$c1.League[mydata$ccode1==367 & mydata$year>1920 & mydata$year<1940]<- 1 # Latvia 1921-1939
mydata$c1.League[mydata$ccode1==450 & mydata$year>1919 & mydata$year<1940]<- 1 # Liberia 1920-1939
mydata$c1.League[mydata$ccode1==368 & mydata$year>1920 & mydata$year<1940]<- 1 # Lithuania 1921-1939
mydata$c1.League[mydata$ccode1==212 & mydata$year>1919 & mydata$year<1940]<- 1 # Luxembourg 1920-1939
mydata$c1.League[mydata$ccode1==70 & mydata$year>1930 & mydata$year<1940]<- 1 # Mexico 1931-1939
mydata$c1.League[mydata$ccode1==210 & mydata$year>1919 & mydata$year<1940]<- 1 # Netherlands 1920-1939
mydata$c1.League[mydata$ccode1==920 & mydata$year>1919 & mydata$year<1940]<- 1 # New Zealand 1920-1939
mydata$c1.League[mydata$ccode1==93 & mydata$year>1919 & mydata$year<1937]<- 1 # Nicaragua 1920-1936
mydata$c1.League[mydata$ccode1==385 & mydata$year>1919 & mydata$year<1940]<- 1 # Norway 1920-1939
mydata$c1.League[mydata$ccode1==95 & mydata$year>1919 & mydata$year<1940]<- 1 # Panama 1920-1939
mydata$c1.League[mydata$ccode1==150 & mydata$year>1919 & mydata$year<1936]<- 1 # Paraguay 1920-1935
mydata$c1.League[mydata$ccode1==135 & mydata$year>1919 & mydata$year<1940]<- 1 # Peru 1920-1939
mydata$c1.League[mydata$ccode1==290 & mydata$year>1919 & mydata$year<1940]<- 1 # Poland 1920-1939
mydata$c1.League[mydata$ccode1==235 & mydata$year>1919 & mydata$year<1940]<- 1 # Portugal 1920-1939
mydata$c1.League[mydata$ccode1==360 & mydata$year>1919 & mydata$year<1941]<- 1 # Romania 1920-1940 ### NOTE LATE DATE
mydata$c1.League[mydata$ccode1==92 & mydata$year>1919 & mydata$year<1938]<- 1 # El Salvador 1920-1937
mydata$c1.League[mydata$ccode1==560 & mydata$year>1919 & mydata$year<1940]<- 1 # South Africa 1920-1939
mydata$c1.League[mydata$ccode1==230 & mydata$year>1919 & mydata$year<1940]<- 1 # Spain 1920-1939
mydata$c1.League[mydata$ccode1==380 & mydata$year>1919 & mydata$year<1940]<- 1 # Sweden 1920-1939
mydata$c1.League[mydata$ccode1==225 & mydata$year>1919 & mydata$year<1940]<- 1 # Switzerland 1920-1939
mydata$c1.League[mydata$ccode1==800 & mydata$year>1919 & mydata$year<1940]<- 1 # Thailand 1920-1939
mydata$c1.League[mydata$ccode1==640 & mydata$year>1931 & mydata$year<1940]<- 1 # Turkey 1932-1939
mydata$c1.League[mydata$ccode1==365 & mydata$year>1933 & mydata$year<1940]<- 1 # Russia for USSR 1934-1939 ### HOW TO HANDLE USSR? NO CCODE - JUST RUSSIA.
mydata$c1.League[mydata$ccode1==165 & mydata$year>1919 & mydata$year<1940]<- 1 # Uruguay 1920-1939
mydata$c1.League[mydata$ccode1==101 & mydata$year>1919 & mydata$year<1939]<- 1 # Venezuela 1920-1938
mydata$c1.League[mydata$ccode1==345 & mydata$year>1919 & mydata$year<1940]<- 1 # Yugoslavia 1920-1939

mydata$c2.League<- 0
mydata$c2.League[mydata$ccode2==700 & mydata$year>1933 & mydata$year<1940]<- 1 # Afghanistan 1934-1939
mydata$c2.League[mydata$ccode2==339 & mydata$year>1919 & mydata$year<1940]<- 1 # Albania 1920-1939(annexed by Italy April, 12 1939)
mydata$c2.League[mydata$ccode2==160 & mydata$year>1919 & mydata$year<1940]<- 1 # Argentina 1920-1939
mydata$c2.League[mydata$ccode2==900 & mydata$year>1919 & mydata$year<1940]<- 1 # Australia 1920-1939
mydata$c2.League[mydata$ccode2==305 & mydata$year>1919 & mydata$year<1939]<- 1 # Austria 1920-1939 (annexed by Germany April 10, 1938)
mydata$c2.League[mydata$ccode2==211 & mydata$year>1919 & mydata$year<1940]<- 1 # Belgium 1920-1939
mydata$c2.League[mydata$ccode2==145 & mydata$year>1919 & mydata$year<1940]<- 1 # Bolivia 1920-1939
mydata$c2.League[mydata$ccode2==140 & mydata$year>1919 & mydata$year<1927]<- 1 # Brazil 1920-1926
mydata$c2.League[mydata$ccode2==200 & mydata$year>1919 & mydata$year<1940]<- 1 # United Kingdom 1920-1939 # NOTE THAT BRITISH EMPIRE WAS THE SAME -- NEED TO ADD THEM. 
mydata$c2.League[mydata$ccode2==355 & mydata$year>1919 & mydata$year<1940]<- 1 # Bulgaria 1920-1939
mydata$c2.League[mydata$ccode2==20 & mydata$year>1919 & mydata$year<1940]<- 1 # Canada 1920-1939
mydata$c2.League[mydata$ccode2==155 & mydata$year>1919 & mydata$year<1939]<- 1 # Chile 1920-1938
mydata$c2.League[mydata$ccode2==710 & mydata$year>1919 & mydata$year<1940]<- 1 # China 1920-1939
mydata$c2.League[mydata$ccode2==100 & mydata$year>1919 & mydata$year<1940]<- 1 # Colombia 1920-1939
mydata$c2.League[mydata$ccode2==94 & mydata$year>1919 & mydata$year<1926]<- 1 # Costa Rica 1920-1925
mydata$c2.League[mydata$ccode2==40 & mydata$year>1919 & mydata$year<1940]<- 1 # Cuba 1920-1939
mydata$c2.League[mydata$ccode2==315 & mydata$year>1919 & mydata$year<1940]<- 1 # Czechoslovakia 1920-1939 (annexed by Germany March 15, 1939)
mydata$c2.League[mydata$ccode2==390 & mydata$year>1919 & mydata$year<1940]<- 1 # Denmark 1920-1939
mydata$c2.League[mydata$ccode2==42 & mydata$year>1923 & mydata$year<1940]<- 1 # Dominican Republic 1924-1939
mydata$c2.League[mydata$ccode2==130 & mydata$year>1933 & mydata$year<1940]<- 1 # Ecuador 1934-1939
mydata$c2.League[mydata$ccode2==651 & mydata$year>1936 & mydata$year<1940]<- 1 # Egypt 1937-1939
mydata$c2.League[mydata$ccode2==366 & mydata$year>1920 & mydata$year<1940]<- 1 # Estonia 1921-1939
mydata$c2.League[mydata$ccode2==530 & mydata$year>1922 & mydata$year<1937]<- 1 # Ethiopia 1923-1936 (annexed by Italy May 9, 1936)
mydata$c2.League[mydata$ccode2==375 & mydata$year>1919 & mydata$year<1940]<- 1 # Finland 1920-1939
mydata$c2.League[mydata$ccode2==220 & mydata$year>1919 & mydata$year<1942]<- 1 # France 1920-1941 ### WHY WAS THIS LATER? DID THEY JUST NOT FORMALLY WITHDRAW UNTIL THEN?
mydata$c2.League[mydata$ccode2==255 & mydata$year>1925 & mydata$year<1934]<- 1 # Germany 1926-1933
mydata$c2.League[mydata$ccode2==350 & mydata$year>1919 & mydata$year<1940]<- 1 # Greece 1920-1939
mydata$c2.League[mydata$ccode2==90 & mydata$year>1919 & mydata$year<1937]<- 1 # Guatemala 1920-1936
mydata$c2.League[mydata$ccode2==41 & mydata$year>1919 & mydata$year<1940]<- 1 # Haiti 1920-1942 ### CHECK THIS END DATE
mydata$c2.League[mydata$ccode2==91 & mydata$year>1919 & mydata$year<1937]<- 1 # Honduras 1920-1936
mydata$c2.League[mydata$ccode2==310 & mydata$year>1921 & mydata$year<1940]<- 1 # Hungary 1922-1939
mydata$c2.League[mydata$ccode2==750 & mydata$year>1919 & mydata$year<1940]<- 1 # India 1920-1939
mydata$c2.League[mydata$ccode2==630 & mydata$year>1919 & mydata$year<1940]<- 1 # Iran 1920-1939
mydata$c2.League[mydata$ccode2==645 & mydata$year>1931 & mydata$year<1940]<- 1 # Iraq 1932-1939
mydata$c2.League[mydata$ccode2==205 & mydata$year>1920 & mydata$year<1940]<- 1 # Ireland gained independence from Britain in 1921.
mydata$c2.League[mydata$ccode2==325 & mydata$year>1919 & mydata$year<1938]<- 1 # Italy 1920-1937
mydata$c2.League[mydata$ccode2==740 & mydata$year>1919 & mydata$year<1934]<- 1 # Japan 1920-1933
mydata$c2.League[mydata$ccode2==367 & mydata$year>1920 & mydata$year<1940]<- 1 # Latvia 1921-1939
mydata$c2.League[mydata$ccode2==450 & mydata$year>1919 & mydata$year<1940]<- 1 # Liberia 1920-1939
mydata$c2.League[mydata$ccode2==368 & mydata$year>1920 & mydata$year<1940]<- 1 # Lithuania 1921-1939
mydata$c2.League[mydata$ccode2==212 & mydata$year>1919 & mydata$year<1940]<- 1 # Luxembourg 1920-1939
mydata$c2.League[mydata$ccode2==70 & mydata$year>1930 & mydata$year<1940]<- 1 # Mexico 1931-1939
mydata$c2.League[mydata$ccode2==210 & mydata$year>1919 & mydata$year<1940]<- 1 # Netherlands 1920-1939
mydata$c2.League[mydata$ccode2==920 & mydata$year>1919 & mydata$year<1940]<- 1 # New Zealand 1920-1939
mydata$c2.League[mydata$ccode2==93 & mydata$year>1919 & mydata$year<1937]<- 1 # Nicaragua 1920-1936
mydata$c2.League[mydata$ccode2==385 & mydata$year>1919 & mydata$year<1940]<- 1 # Norway 1920-1939
mydata$c2.League[mydata$ccode2==95 & mydata$year>1919 & mydata$year<1940]<- 1 # Panama 1920-1939
mydata$c2.League[mydata$ccode2==150 & mydata$year>1919 & mydata$year<1936]<- 1 # Paraguay 1920-1935
mydata$c2.League[mydata$ccode2==135 & mydata$year>1919 & mydata$year<1940]<- 1 # Peru 1920-1939
mydata$c2.League[mydata$ccode2==290 & mydata$year>1919 & mydata$year<1940]<- 1 # Poland 1920-1939
mydata$c2.League[mydata$ccode2==235 & mydata$year>1919 & mydata$year<1940]<- 1 # Portugal 1920-1939
mydata$c2.League[mydata$ccode2==360 & mydata$year>1919 & mydata$year<1941]<- 1 # Romania 1920-1940 ### NOTE LATE DATE
mydata$c2.League[mydata$ccode2==92 & mydata$year>1919 & mydata$year<1938]<- 1 # El Salvador 1920-1937
mydata$c2.League[mydata$ccode2==560 & mydata$year>1919 & mydata$year<1940]<- 1 # South Africa 1920-1939
mydata$c2.League[mydata$ccode2==230 & mydata$year>1919 & mydata$year<1940]<- 1 # Spain 1920-1939
mydata$c2.League[mydata$ccode2==380 & mydata$year>1919 & mydata$year<1940]<- 1 # Sweden 1920-1939
mydata$c2.League[mydata$ccode2==225 & mydata$year>1919 & mydata$year<1940]<- 1 # Switzerland 1920-1939
mydata$c2.League[mydata$ccode2==800 & mydata$year>1919 & mydata$year<1940]<- 1 # Thailand 1920-1939
mydata$c2.League[mydata$ccode2==640 & mydata$year>1931 & mydata$year<1940]<- 1 # Turkey 1932-1939
mydata$c2.League[mydata$ccode2==365 & mydata$year>1933 & mydata$year<1940]<- 1 # Russia for USSR 1934-1939 ### HOW TO HANDLE USSR? NO CCODE - JUST RUSSIA.
mydata$c2.League[mydata$ccode2==165 & mydata$year>1919 & mydata$year<1940]<- 1 # Uruguay 1920-1939
mydata$c2.League[mydata$ccode2==101 & mydata$year>1919 & mydata$year<1939]<- 1 # Venezuela 1920-1938
mydata$c2.League[mydata$ccode2==345 & mydata$year>1919 & mydata$year<1940]<- 1 # Yugoslavia 1920-1939

# Post World War II Liberal Order from 1945 - 1991 (i.e., "First World" aligned against the Soviet Union) see: http://www.nato.int/cps/en/natohq/topics_52044.htm

# Below are NATO Members through 1991; states aligned with the West but not in NATO; and neutral states that are liberal, democratic. 

mydata$c1.PostWarLiberal<-0
mydata$c1.PostWarLiberal[mydata$ccode1==211 & mydata$year>1944 & mydata$year<1992]<- 1 # Belgium
mydata$c1.PostWarLiberal[mydata$ccode1==20 & mydata$year>1944 & mydata$year<1992]<- 1 # Canada
mydata$c1.PostWarLiberal[mydata$ccode1==390 & mydata$year>1944 & mydata$year<1992]<- 1 # Denmark
mydata$c1.PostWarLiberal[mydata$ccode1==220 & mydata$year>1944 & mydata$year<1992]<- 1 # France
mydata$c1.PostWarLiberal[mydata$ccode1==395 & mydata$year>1944 & mydata$year<1992]<- 1 # Iceland
mydata$c1.PostWarLiberal[mydata$ccode1==325 & mydata$year>1944 & mydata$year<1992]<- 1 # Italy
mydata$c1.PostWarLiberal[mydata$ccode1==212 & mydata$year>1944 & mydata$year<1992]<- 1 # Luxembourg
mydata$c1.PostWarLiberal[mydata$ccode1==210 & mydata$year>1944 & mydata$year<1992]<- 1 # Netherlands
mydata$c1.PostWarLiberal[mydata$ccode1==385 & mydata$year>1944 & mydata$year<1992]<- 1 # Norway
mydata$c1.PostWarLiberal[mydata$ccode1==235 & mydata$year>1944 & mydata$year<1992]<- 1 # Portugal
mydata$c1.PostWarLiberal[mydata$ccode1==200 & mydata$year>1944 & mydata$year<1992]<- 1 # United Kingdom
mydata$c1.PostWarLiberal[mydata$ccode1==2 & mydata$year>1944 & mydata$year<1992]<- 1 # United States
mydata$c1.PostWarLiberal[mydata$ccode1==350 & mydata$year>1951 & mydata$year<1992]<- 1 # Greece joined NATO in 1952
mydata$c1.PostWarLiberal[mydata$ccode1==640 & mydata$year>1951 & mydata$year<1992]<- 1 # Turkey joined NATO in 1952
mydata$c1.PostWarLiberal[mydata$ccode1==260 & mydata$year>1954 & mydata$year<1991]<- 1 # German Federal Republic joined NATO in 1955 and was a member through October 1990
mydata$c1.PostWarLiberal[mydata$ccode1==255 & mydata$year>1989 & mydata$year<1992]<- 1 # Unified Germany joined NATO in October 1990 ## note overlap with W. Germany in 1990.
mydata$c1.PostWarLiberal[mydata$ccode1==230 & mydata$year>1981 & mydata$year<1992]<- 1 # Spain joined NATO in 1982
mydata$c1.PostWarLiberal[mydata$ccode1==666 & mydata$year>1947 & mydata$year<1992]<- 1 # Israel founded in 1948
mydata$c1.PostWarLiberal[mydata$ccode1==740 & mydata$year>1951 & mydata$year<1992]<- 1 # Japan: US occupation ended in 1952. See: http://afe.easia.columbia.edu/special/japan_1900_occupation.htm
mydata$c1.PostWarLiberal[mydata$ccode1==732 & mydata$year>1947 & mydata$year<1992]<- 1 # Republic of Korea. US occupation ended in 1948. Note CCODE 732 is the ROK specific code. 
mydata$c1.PostWarLiberal[mydata$ccode1==900 & mydata$year>1944 & mydata$year<1992]<- 1 # Australia. Note that the ANZUS treaty started in 1951 but Australia was aligned with West prior. 
mydata$c1.PostWarLiberal[mydata$ccode1==920 & mydata$year>1944 & mydata$year<1992]<- 1 # New Zealand. Note that the ANZUS treaty started in 1951 but NZ was aligned with West prior. 
mydata$c1.PostWarLiberal[mydata$ccode1==305 & mydata$year>1944 & mydata$year<1992]<- 1 # Austria was western, liberal democratic but neutral. Note CCODE 305 is the Austria-specific code. There is a separate Austria-Hungary code (300)
mydata$c1.PostWarLiberal[mydata$ccode1==205 & mydata$year>1944 & mydata$year<1992]<- 1 # Ireland was western, liberal democratic but neutral.
mydata$c1.PostWarLiberal[mydata$ccode1==380 & mydata$year>1944 & mydata$year<1992]<- 1 # Sweden was western, liberal democratic but neutral.
mydata$c1.PostWarLiberal[mydata$ccode1==225 & mydata$year>1944 & mydata$year<1992]<- 1 # Switzerland was western, liberal democratic but neutral.
mydata$c1.PostWarLiberal[mydata$ccode1==817 & mydata$year>1954 & mydata$year<1976]<- 1 # Republic of ("South") Vietnam as of 1955.
mydata$c1.PostWarLiberal[mydata$ccode1==713 & mydata$year>1946 & mydata$year<1992]<- 1 # Republic of China ("Taiwan") concluded its consitution in 1947. 


mydata$c2.PostWarLiberal<-0
mydata$c2.PostWarLiberal[mydata$ccode2==211 & mydata$year>1944 & mydata$year<1992]<- 1 # Belgium
mydata$c2.PostWarLiberal[mydata$ccode2==20 & mydata$year>1944 & mydata$year<1992]<- 1 # Canada
mydata$c2.PostWarLiberal[mydata$ccode2==390 & mydata$year>1944 & mydata$year<1992]<- 1 # Denmark
mydata$c2.PostWarLiberal[mydata$ccode2==220 & mydata$year>1944 & mydata$year<1992]<- 1 # France
mydata$c2.PostWarLiberal[mydata$ccode2==395 & mydata$year>1944 & mydata$year<1992]<- 1 # Iceland
mydata$c2.PostWarLiberal[mydata$ccode2==325 & mydata$year>1944 & mydata$year<1992]<- 1 # Italy
mydata$c2.PostWarLiberal[mydata$ccode2==212 & mydata$year>1944 & mydata$year<1992]<- 1 # Luxembourg
mydata$c2.PostWarLiberal[mydata$ccode2==210 & mydata$year>1944 & mydata$year<1992]<- 1 # Netherlands
mydata$c2.PostWarLiberal[mydata$ccode2==385 & mydata$year>1944 & mydata$year<1992]<- 1 # Norway
mydata$c2.PostWarLiberal[mydata$ccode2==235 & mydata$year>1944 & mydata$year<1992]<- 1 # Portugal
mydata$c2.PostWarLiberal[mydata$ccode2==200 & mydata$year>1944 & mydata$year<1992]<- 1 # United Kingdom
mydata$c2.PostWarLiberal[mydata$ccode2==2 & mydata$year>1944 & mydata$year<1992]<- 1 # United States
mydata$c2.PostWarLiberal[mydata$ccode2==350 & mydata$year>1951 & mydata$year<1992]<- 1 # Greece joined NATO in 1952
mydata$c2.PostWarLiberal[mydata$ccode2==640 & mydata$year>1951 & mydata$year<1992]<- 1 # Turkey joined NATO in 1952
mydata$c2.PostWarLiberal[mydata$ccode2==260 & mydata$year>1954 & mydata$year<1991]<- 1 # German Federal Republic joined NATO in 1955 and was a member through October 1990
mydata$c2.PostWarLiberal[mydata$ccode2==255 & mydata$year>1989 & mydata$year<1992]<- 1 # Unified Germany joined NATO in October 1990 ## note overlap with W. Germany in 1990.
mydata$c2.PostWarLiberal[mydata$ccode2==230 & mydata$year>1981 & mydata$year<1992]<- 1 # Spain joined NATO in 1982
mydata$c2.PostWarLiberal[mydata$ccode2==666 & mydata$year>1947 & mydata$year<1992]<- 1 # Israel founded in 1948
mydata$c2.PostWarLiberal[mydata$ccode2==740 & mydata$year>1951 & mydata$year<1992]<- 1 # Japan: US occupation ended in 1952. See: http://afe.easia.columbia.edu/special/japan_1900_occupation.htm
mydata$c2.PostWarLiberal[mydata$ccode2==732 & mydata$year>1947 & mydata$year<1992]<- 1 # Republic of Korea. US occupation ended in 1948. Note CCODE 732 is the ROK specific code.
mydata$c2.PostWarLiberal[mydata$ccode2==900 & mydata$year>1944 & mydata$year<1992]<- 1 # Australia. Note that the ANZUS treaty started in 1951 but Australia was aligned with West prior. 
mydata$c2.PostWarLiberal[mydata$ccode2==920 & mydata$year>1944 & mydata$year<1992]<- 1 # New Zealand. Note that the ANZUS treaty started in 1951 but NZ was aligned with West prior. 
mydata$c2.PostWarLiberal[mydata$ccode2==305 & mydata$year>1944 & mydata$year<1992]<- 1 # Austria was western, liberal democratic but neutral. Note CCODE 305 is the Austria-specific code. There is a separate Austria-Hungary code (300)
mydata$c2.PostWarLiberal[mydata$ccode2==205 & mydata$year>1944 & mydata$year<1992]<- 1 # Ireland was western, liberal democratic but neutral.
mydata$c2.PostWarLiberal[mydata$ccode2==380 & mydata$year>1944 & mydata$year<1992]<- 1 # Sweden was western, liberal democratic but neutral.
mydata$c2.PostWarLiberal[mydata$ccode2==225 & mydata$year>1944 & mydata$year<1992]<- 1 # Switzerland was western, liberal democratic but neutral.
mydata$c2.PostWarLiberal[mydata$ccode2==817 & mydata$year>1954 & mydata$year<1976]<- 1 # Republic of ("South") Vietnam as of 1955. 
mydata$c2.PostWarLiberal[mydata$ccode2==713 & mydata$year>1946 & mydata$year<1992]<- 1 # Republic of China ("Taiwan") concluded its consitution in 1947. 


# Post World War II Communist Order from 1945 - 1991 (i.e., "Second World" aligned against NATO) See: https://history.state.gov/milestones/1953-1960/warsaw-treaty 
# for Warsaw Pact countries.  Note that Soviet states do not have individual year dyad information in the dataset. 

# Below are Warsaw Pact States, other communist states, and states aligned with the Soviet sphere but not formally allied. 

mydata$c1.PostWarCommunist<-0
mydata$c1.PostWarCommunist[mydata$ccode1==339 & mydata$year>1954 & mydata$year<1992]<- 1 # Albania
mydata$c1.PostWarCommunist[mydata$ccode1==100 & mydata$year>1954 & mydata$year<1992]<- 1 # Bulgaria
mydata$c1.PostWarCommunist[mydata$ccode1==315 & mydata$year>1954 & mydata$year<1992]<- 1 # Czechoslovakia
mydata$c1.PostWarCommunist[mydata$ccode1==265 & mydata$year>1954 & mydata$year<1991]<- 1 # German Democratic Republic through 1990
mydata$c1.PostWarCommunist[mydata$ccode1==310 & mydata$year>1954 & mydata$year<1992]<- 1 # Hungary
mydata$c1.PostWarCommunist[mydata$ccode1==290 & mydata$year>1954 & mydata$year<1992]<- 1 # Poland
mydata$c1.PostWarCommunist[mydata$ccode1==360 & mydata$year>1954 & mydata$year<1992]<- 1 # Romania
mydata$c1.PostWarCommunist[mydata$ccode1==365 & mydata$year>1954 & mydata$year<1992]<- 1 # Russia
# mydata$c1.PostWarCommunist[mydata$ccode1==375 & mydata$year>1944 & mydata$year<1992]<- 1 # Finland was neutral but leaned toward the Soviets in the latter part of the Cold War. 
mydata$c1.PostWarCommunist[mydata$ccode1==710 & mydata$year>1948 & mydata$year<1992]<- 1 # People's Republic of China founded in 1949
mydata$c1.PostWarCommunist[mydata$ccode1==731 & mydata$year>1947 & mydata$year<1992]<- 1 # Democratic People's Republic of Korea founded in 1948. Previously Soviet occupied.
mydata$c1.PostWarCommunist[mydata$ccode1==812 & mydata$year>1948 & mydata$year<1992]<- 1 # Lao Peoples Democratic Republic gained independence in 1949.
mydata$c1.PostWarCommunist[mydata$ccode1==712 & mydata$year>1944 & mydata$year<1992]<- 1 # Mongolia
mydata$c1.PostWarCommunist[mydata$ccode1==816 & mydata$year>1944 & mydata$year<1992]<- 1 # Socialist Republic of Vietnam gained independence in 1945.
mydata$c1.PostWarCommunist[mydata$ccode1==371 & mydata$year>1944 & mydata$year<1992]<- 1 # Armenia was part of the Soviet Union.
mydata$c1.PostWarCommunist[mydata$ccode1==373 & mydata$year>1944 & mydata$year<1992]<- 1 # Azerbaijan was part of the Soviet Union.
mydata$c1.PostWarCommunist[mydata$ccode1==370 & mydata$year>1944 & mydata$year<1992]<- 1 # Belarus was part of the Soviet Union.
mydata$c1.PostWarCommunist[mydata$ccode1==366 & mydata$year>1944 & mydata$year<1992]<- 1 # Estonia was part of the Soviet Union.
mydata$c1.PostWarCommunist[mydata$ccode1==372 & mydata$year>1944 & mydata$year<1992]<- 1 # Georgia was part of the Soviet Union.
mydata$c1.PostWarCommunist[mydata$ccode1==705 & mydata$year>1944 & mydata$year<1992]<- 1 # Kazakhstan was part of the Soviet Union.
mydata$c1.PostWarCommunist[mydata$ccode1==703 & mydata$year>1944 & mydata$year<1992]<- 1 # Kyrgyzstan was part of the Soviet Union.
mydata$c1.PostWarCommunist[mydata$ccode1==367 & mydata$year>1944 & mydata$year<1992]<- 1 # Latvia was part of the Soviet Union.
mydata$c1.PostWarCommunist[mydata$ccode1==368 & mydata$year>1944 & mydata$year<1992]<- 1 # Lithuania was part of the Soviet Union.
mydata$c1.PostWarCommunist[mydata$ccode1==359 & mydata$year>1944 & mydata$year<1992]<- 1 # Moldova was part of the Soviet Union.
mydata$c1.PostWarCommunist[mydata$ccode1==702 & mydata$year>1944 & mydata$year<1992]<- 1 # Tajikistan was part of the Soviet Union.
mydata$c1.PostWarCommunist[mydata$ccode1==701 & mydata$year>1944 & mydata$year<1992]<- 1 # Turkmenistan was part of the Soviet Union.
mydata$c1.PostWarCommunist[mydata$ccode1==369 & mydata$year>1944 & mydata$year<1992]<- 1 # Ukraine was part of the Soviet Union.
mydata$c1.PostWarCommunist[mydata$ccode1==704 & mydata$year>1944 & mydata$year<1992]<- 1 # Uzbekistan was part of the Soviet Union.

mydata$c2.PostWarCommunist<-0
mydata$c2.PostWarCommunist[mydata$ccode2==339 & mydata$year>1954 & mydata$year<1992]<- 1 # Albania
mydata$c2.PostWarCommunist[mydata$ccode2==100 & mydata$year>1954 & mydata$year<1992]<- 1 # Bulgaria
mydata$c2.PostWarCommunist[mydata$ccode2==315 & mydata$year>1954 & mydata$year<1992]<- 1 # Czechoslovakia
mydata$c2.PostWarCommunist[mydata$ccode2==265 & mydata$year>1954 & mydata$year<1991]<- 1 # German Democratic Republic through 1990
mydata$c2.PostWarCommunist[mydata$ccode2==310 & mydata$year>1954 & mydata$year<1992]<- 1 # Hungary
mydata$c2.PostWarCommunist[mydata$ccode2==290 & mydata$year>1954 & mydata$year<1992]<- 1 # Poland
mydata$c2.PostWarCommunist[mydata$ccode2==360 & mydata$year>1954 & mydata$year<1992]<- 1 # Romania
mydata$c2.PostWarCommunist[mydata$ccode2==365 & mydata$year>1954 & mydata$year<1992]<- 1 # Russia
# mydata$c2.PostWarCommunist[mydata$ccode2==375 & mydata$year>1944 & mydata$year<1992]<- 1 # Finland was neutral but leaned toward the Soviets in the latter part of the Cold War. 
mydata$c2.PostWarCommunist[mydata$ccode2==710 & mydata$year>1948 & mydata$year<1992]<- 1 # People's Republic of China founded in 1949
mydata$c2.PostWarCommunist[mydata$ccode2==731 & mydata$year>1947 & mydata$year<1992]<- 1 # Democratic People's Republic of Korea founded in 1948. Previously Soviet occupied.
mydata$c2.PostWarCommunist[mydata$ccode2==812 & mydata$year>1948 & mydata$year<1992]<- 1 # Lao Peoples Democratic Republic gained independence in 1949.
mydata$c2.PostWarCommunist[mydata$ccode2==712 & mydata$year>1944 & mydata$year<1992]<- 1 # Mongolia
mydata$c2.PostWarCommunist[mydata$ccode2==816 & mydata$year>1944 & mydata$year<1992]<- 1 # Socialist Republic of Vietnam gained independence in 1945.
mydata$c2.PostWarCommunist[mydata$ccode2==371 & mydata$year>1944 & mydata$year<1992]<- 1 # Armenia was part of the Soviet Union.
mydata$c2.PostWarCommunist[mydata$ccode2==373 & mydata$year>1944 & mydata$year<1992]<- 1 # Azerbaijan was part of the Soviet Union.
mydata$c2.PostWarCommunist[mydata$ccode2==370 & mydata$year>1944 & mydata$year<1992]<- 1 # Belarus was part of the Soviet Union.
mydata$c2.PostWarCommunist[mydata$ccode2==366 & mydata$year>1944 & mydata$year<1992]<- 1 # Estonia was part of the Soviet Union.
mydata$c2.PostWarCommunist[mydata$ccode2==372 & mydata$year>1944 & mydata$year<1992]<- 1 # Georgia was part of the Soviet Union.
mydata$c2.PostWarCommunist[mydata$ccode2==705 & mydata$year>1944 & mydata$year<1992]<- 1 # Kazakhstan was part of the Soviet Union.
mydata$c2.PostWarCommunist[mydata$ccode2==703 & mydata$year>1944 & mydata$year<1992]<- 1 # Kyrgyzstan was part of the Soviet Union.
mydata$c2.PostWarCommunist[mydata$ccode2==367 & mydata$year>1944 & mydata$year<1992]<- 1 # Latvia was part of the Soviet Union.
mydata$c2.PostWarCommunist[mydata$ccode2==368 & mydata$year>1944 & mydata$year<1992]<- 1 # Lithuania was part of the Soviet Union.
mydata$c2.PostWarCommunist[mydata$ccode2==359 & mydata$year>1944 & mydata$year<1992]<- 1 # Moldova was part of the Soviet Union.
mydata$c2.PostWarCommunist[mydata$ccode2==702 & mydata$year>1944 & mydata$year<1992]<- 1 # Tajikistan was part of the Soviet Union.
mydata$c2.PostWarCommunist[mydata$ccode2==701 & mydata$year>1944 & mydata$year<1992]<- 1 # Turkmenistan was part of the Soviet Union.
mydata$c2.PostWarCommunist[mydata$ccode2==369 & mydata$year>1944 & mydata$year<1992]<- 1 # Ukraine was part of the Soviet Union.
mydata$c2.PostWarCommunist[mydata$ccode2==704 & mydata$year>1944 & mydata$year<1992]<- 1 # Uzbekistan was part of the Soviet Union.

# Warsaw Pact countries
mydata$c1.WarsawPact<-0
mydata$c1.WarsawPact[mydata$ccode1==339 & mydata$year>1954 & mydata$year<1969]<- 1 # Albania through 1968
mydata$c1.WarsawPact[mydata$ccode1==100 & mydata$year>1954 & mydata$year<1992]<- 1 # Bulgaria
mydata$c1.WarsawPact[mydata$ccode1==315 & mydata$year>1954 & mydata$year<1992]<- 1 # Czechoslovakia
mydata$c1.WarsawPact[mydata$ccode1==265 & mydata$year>1954 & mydata$year<1991]<- 1 # German Democratic Republic through 1990
mydata$c1.WarsawPact[mydata$ccode1==310 & mydata$year>1954 & mydata$year<1992]<- 1 # Hungary
mydata$c1.WarsawPact[mydata$ccode1==290 & mydata$year>1954 & mydata$year<1992]<- 1 # Poland
mydata$c1.WarsawPact[mydata$ccode1==360 & mydata$year>1954 & mydata$year<1992]<- 1 # Romania
mydata$c1.WarsawPact[mydata$ccode1==365 & mydata$year>1954 & mydata$year<1992]<- 1 # Russia

mydata$c2.WarsawPact<-0
mydata$c2.WarsawPact[mydata$ccode2==339 & mydata$year>1954 & mydata$year<1969]<- 1 # Albania through 1968
mydata$c2.WarsawPact[mydata$ccode2==100 & mydata$year>1954 & mydata$year<1992]<- 1 # Bulgaria
mydata$c2.WarsawPact[mydata$ccode2==315 & mydata$year>1954 & mydata$year<1992]<- 1 # Czechoslovakia
mydata$c2.WarsawPact[mydata$ccode2==265 & mydata$year>1954 & mydata$year<1991]<- 1 # German Democratic Republic through 1990
mydata$c2.WarsawPact[mydata$ccode2==310 & mydata$year>1954 & mydata$year<1992]<- 1 # Hungary
mydata$c2.WarsawPact[mydata$ccode2==290 & mydata$year>1954 & mydata$year<1992]<- 1 # Poland
mydata$c2.WarsawPact[mydata$ccode2==360 & mydata$year>1954 & mydata$year<1992]<- 1 # Romania
mydata$c2.WarsawPact[mydata$ccode2==365 & mydata$year>1954 & mydata$year<1992]<- 1 # Russia


# Post World War II states that were in neither camp.  This includes both un-aligned middle powers and those states previously referred to as the "third world." 

mydata$c1.PostWarOther<-0
mydata$c1.PostWarOther[mydata$c1.PostWarCommunist==0 & mydata$c1.PostWarLiberal==0 & mydata$year>1944 & mydata$year<1992]<-1

mydata$c2.PostWarOther<-0
mydata$c2.PostWarOther[mydata$c2.PostWarCommunist==0 & mydata$c2.PostWarLiberal==0 & mydata$year>1944 & mydata$year<1992]<-1

# Post-Cold War Western order (assumed to be end-of-CW coalition)

mydata$c1.PostCWLiberal<-0
mydata$c1.PostCWLiberal[mydata$ccode1==211 & mydata$year>1991]<- 1 # Belgium
mydata$c1.PostCWLiberal[mydata$ccode1==20 & mydata$year>1991]<- 1 # Canada
mydata$c1.PostCWLiberal[mydata$ccode1==390 & mydata$year>1991]<- 1 # Denmark
mydata$c1.PostCWLiberal[mydata$ccode1==220 & mydata$year>1991]<- 1 # France
mydata$c1.PostCWLiberal[mydata$ccode1==395 & mydata$year>1991]<- 1 # Iceland
mydata$c1.PostCWLiberal[mydata$ccode1==325 & mydata$year>1991]<- 1 # Italy
mydata$c1.PostCWLiberal[mydata$ccode1==212 & mydata$year>1991]<- 1 # Luxembourg
mydata$c1.PostCWLiberal[mydata$ccode1==210 & mydata$year>1991]<- 1 # Netherlands
mydata$c1.PostCWLiberal[mydata$ccode1==385 & mydata$year>1991]<- 1 # Norway
mydata$c1.PostCWLiberal[mydata$ccode1==235 & mydata$year>1991]<- 1 # Portugal
mydata$c1.PostCWLiberal[mydata$ccode1==200 & mydata$year>1991]<- 1 # United Kingdom
mydata$c1.PostCWLiberal[mydata$ccode1==2 & mydata$year>1991]<- 1 # United States
mydata$c1.PostCWLiberal[mydata$ccode1==350 & mydata$year>1991]<- 1 # Greece joined NATO in 1952
mydata$c1.PostCWLiberal[mydata$ccode1==640 & mydata$year>1991]<- 1 # Turkey joined NATO in 1952
mydata$c1.PostCWLiberal[mydata$ccode1==260 & mydata$year>1991]<- 1 # German Federal Republic joined NATO in 1955 and was a member through October 1990
mydata$c1.PostCWLiberal[mydata$ccode1==255 & mydata$year>1991]<- 1 # Unified Germany joined NATO in October 1990 ## note overlap with W. Germany in 1990.
mydata$c1.PostCWLiberal[mydata$ccode1==230 & mydata$year>1991]<- 1 # Spain joined NATO in 1982
mydata$c1.PostCWLiberal[mydata$ccode1==666 & mydata$year>1991]<- 1 # Israel founded in 1948
mydata$c1.PostCWLiberal[mydata$ccode1==740 & mydata$year>1991]<- 1 # Japan: US occupation ended in 1952. See: http://afe.easia.columbia.edu/special/japan_1900_occupation.htm
mydata$c1.PostCWLiberal[mydata$ccode1==732 & mydata$year>1991]<- 1 # Republic of Korea. US occupation ended in 1948. Note CCODE 732 is the ROK specific code.
mydata$c1.PostCWLiberal[mydata$ccode1==900 & mydata$year>1991]<- 1 # Australia. Note that the ANZUS treaty started in 1951 but Australia was aligned with West prior. 
mydata$c1.PostCWLiberal[mydata$ccode1==920 & mydata$year>1991]<- 1 # New Zealand. Note that the ANZUS treaty started in 1951 but NZ was aligned with West prior. 
mydata$c1.PostCWLiberal[mydata$ccode1==305 & mydata$year>1991]<- 1 # Austria was western, liberal democratic but neutral. Note CCODE 305 is the Austria-specific code. There is a separate Austria-Hungary code (300)
mydata$c1.PostCWLiberal[mydata$ccode1==205 & mydata$year>1991]<- 1 # Ireland was western, liberal democratic but neutral.
mydata$c1.PostCWLiberal[mydata$ccode1==380 & mydata$year>1991]<- 1 # Sweden was western, liberal democratic but neutral.
mydata$c1.PostCWLiberal[mydata$ccode1==225 & mydata$year>1991]<- 1 # Switzerland was western, liberal democratic but neutral.
mydata$c1.PostCWLiberal[mydata$ccode1==713 & mydata$year>1991]<- 1 # Republic of China ("Taiwan") concluded its consitution in 1947. 



mydata$c2.PostCWLiberal<-0
mydata$c2.PostCWLiberal[mydata$ccode2==211 & mydata$year>1991]<- 1 # Belgium
mydata$c2.PostCWLiberal[mydata$ccode2==20 & mydata$year>1991]<- 1 # Canada
mydata$c2.PostCWLiberal[mydata$ccode2==390 & mydata$year>1991]<- 1 # Denmark
mydata$c2.PostCWLiberal[mydata$ccode2==220 & mydata$year>1991]<- 1 # France
mydata$c2.PostCWLiberal[mydata$ccode2==395 & mydata$year>1991]<- 1 # Iceland
mydata$c2.PostCWLiberal[mydata$ccode2==325 & mydata$year>1991]<- 1 # Italy
mydata$c2.PostCWLiberal[mydata$ccode2==212 & mydata$year>1991]<- 1 # Luxembourg
mydata$c2.PostCWLiberal[mydata$ccode2==210 & mydata$year>1991]<- 1 # Netherlands
mydata$c2.PostCWLiberal[mydata$ccode2==385 & mydata$year>1991]<- 1 # Norway
mydata$c2.PostCWLiberal[mydata$ccode2==235 & mydata$year>1991]<- 1 # Portugal
mydata$c2.PostCWLiberal[mydata$ccode2==200 & mydata$year>1991]<- 1 # United Kingdom
mydata$c2.PostCWLiberal[mydata$ccode2==2 & mydata$year>1991]<- 1 # United States
mydata$c2.PostCWLiberal[mydata$ccode2==350 & mydata$year>1991]<- 1 # Greece joined NATO in 1952
mydata$c2.PostCWLiberal[mydata$ccode2==640 & mydata$year>1991]<- 1 # Turkey joined NATO in 1952
mydata$c2.PostCWLiberal[mydata$ccode2==260 & mydata$year>1991]<- 1 # German Federal Republic joined NATO in 1955 and was a member through October 1990
mydata$c2.PostCWLiberal[mydata$ccode2==255 & mydata$year>1991]<- 1 # Unified Germany joined NATO in October 1990 ## note overlap with W. Germany in 1990.
mydata$c2.PostCWLiberal[mydata$ccode2==230 & mydata$year>1991]<- 1 # Spain joined NATO in 1982
mydata$c2.PostCWLiberal[mydata$ccode2==666 & mydata$year>1991]<- 1 # Israel founded in 1948
mydata$c2.PostCWLiberal[mydata$ccode2==740 & mydata$year>1991]<- 1 # Japan: US occupation ended in 1952. See: http://afe.easia.columbia.edu/special/japan_1900_occupation.htm
mydata$c2.PostCWLiberal[mydata$ccode2==732 & mydata$year>1991]<- 1 # Republic of Korea. US occupation ended in 1948. Note CCODE 732 is the ROK specific code.
mydata$c2.PostCWLiberal[mydata$ccode2==900 & mydata$year>1991]<- 1 # Australia. Note that the ANZUS treaty started in 1951 but Australia was aligned with West prior. 
mydata$c2.PostCWLiberal[mydata$ccode2==920 & mydata$year>1991]<- 1 # New Zealand. Note that the ANZUS treaty started in 1951 but NZ was aligned with West prior. 
mydata$c2.PostCWLiberal[mydata$ccode2==305 & mydata$year>1991]<- 1 # Austria was western, liberal democratic but neutral. Note CCODE 305 is the Austria-specific code. There is a separate Austria-Hungary code (300)
mydata$c2.PostCWLiberal[mydata$ccode2==205 & mydata$year>1991]<- 1 # Ireland was western, liberal democratic but neutral.
mydata$c2.PostCWLiberal[mydata$ccode2==380 & mydata$year>1991]<- 1 # Sweden was western, liberal democratic but neutral.
mydata$c2.PostCWLiberal[mydata$ccode2==225 & mydata$year>1991]<- 1 # Switzerland was western, liberal democratic but neutral.
mydata$c2.PostCWLiberal[mydata$ccode2==713 & mydata$year>1991]<- 1 # Republic of China ("Taiwan") concluded its consitution in 1947. 

# NATO expansion countries; coding based on status as of 1/1/year
mydata$c1.PostCWLiberal[mydata$ccode1==316 & mydata$year>1999]<- 1 # Czech Republic
mydata$c1.PostCWLiberal[mydata$ccode1==290 & mydata$year>1999]<- 1 # Poland
mydata$c1.PostCWLiberal[mydata$ccode1==310 & mydata$year>1999]<- 1 # Hungary
mydata$c1.PostCWLiberal[mydata$ccode1==355 & mydata$year>2004]<- 1 # Bulgaria
mydata$c1.PostCWLiberal[mydata$ccode1==366 & mydata$year>2004]<- 1 # Estonia
mydata$c1.PostCWLiberal[mydata$ccode1==367 & mydata$year>2004]<- 1 # Latvia
mydata$c1.PostCWLiberal[mydata$ccode1==368 & mydata$year>2004]<- 1 # Lithuania
mydata$c1.PostCWLiberal[mydata$ccode1==360 & mydata$year>2004]<- 1 # Romania
mydata$c1.PostCWLiberal[mydata$ccode1==317 & mydata$year>2004]<- 1 # Slovakia
mydata$c1.PostCWLiberal[mydata$ccode1==349 & mydata$year>2004]<- 1 # Slovenia
mydata$c1.PostCWLiberal[mydata$ccode1==339 & mydata$year>2009]<- 1 # Albania
mydata$c1.PostCWLiberal[mydata$ccode1==344 & mydata$year>2009]<- 1 # Croatia
mydata$c2.PostCWLiberal[mydata$ccode2==316 & mydata$year>1999]<- 1 # Czech Republic
mydata$c2.PostCWLiberal[mydata$ccode2==290 & mydata$year>1999]<- 1 # Poland
mydata$c2.PostCWLiberal[mydata$ccode2==310 & mydata$year>1999]<- 1 # Hungary
mydata$c2.PostCWLiberal[mydata$ccode2==355 & mydata$year>2004]<- 1 # Bulgaria
mydata$c2.PostCWLiberal[mydata$ccode2==366 & mydata$year>2004]<- 1 # Estonia
mydata$c2.PostCWLiberal[mydata$ccode2==367 & mydata$year>2004]<- 1 # Latvia
mydata$c2.PostCWLiberal[mydata$ccode2==368 & mydata$year>2004]<- 1 # Lithuania
mydata$c2.PostCWLiberal[mydata$ccode2==360 & mydata$year>2004]<- 1 # Romania
mydata$c2.PostCWLiberal[mydata$ccode2==317 & mydata$year>2004]<- 1 # Slovakia
mydata$c2.PostCWLiberal[mydata$ccode2==349 & mydata$year>2004]<- 1 # Slovenia
mydata$c2.PostCWLiberal[mydata$ccode2==339 & mydata$year>2009]<- 1 # Albania
mydata$c2.PostCWLiberal[mydata$ccode2==344 & mydata$year>2009]<- 1 # Croatia
# mydata$c2.PostCWLiberal[mydata$ccode2==341 & mydata$year>2017]<- 1 # Montenegro

# EU expansion (not including those predated by NATO expansion); 1/1/year
mydata$c1.PostCWLiberal[mydata$ccode1==375 & mydata$year>1994]<- 1 # Finland
mydata$c1.PostCWLiberal[mydata$ccode1==352 & mydata$year>2004]<- 1 # Cyprus
mydata$c1.PostCWLiberal[mydata$ccode1==338 & mydata$year>2004]<- 1 # Malta
mydata$c2.PostCWLiberal[mydata$ccode2==375 & mydata$year>1994]<- 1 # Finland
mydata$c2.PostCWLiberal[mydata$ccode2==352 & mydata$year>2004]<- 1 # Cyprus
mydata$c2.PostCWLiberal[mydata$ccode2==338 & mydata$year>2004]<- 1 # Malta


mydata$order <- 0  
mydata$order[(mydata$c1.concert==1 & mydata$c2.concert==1) | (mydata$c1.bismarck==1 & mydata$c2.bismarck==1) | (mydata$c1.League==1 & mydata$c2.League==1) | (mydata$c1.PostWarLiberal==1 & mydata$c2.PostWarLiberal==1) | (mydata$c1.PostWarCommunist==1 & mydata$c2.PostWarCommunist==1) | (mydata$c1.PostCWLiberal==1 & mydata$c2.PostCWLiberal==1)] <- 1

mydata$outsideorder <- 0  
mydata$outsideorder[(mydata$c1.concert==1 & mydata$c2.concert==0) | (mydata$c1.bismarck==1 & mydata$c2.bismarck==0) | (mydata$c1.League==1 & mydata$c2.League==0 & !(mydata$ccode2 %in% c(255,365,740))) | (mydata$c1.concert==0 & mydata$c2.concert==1) | (mydata$c1.bismarck==0 & mydata$c2.bismarck==1) | (mydata$c1.League==0 & !(mydata$ccode1 %in% c(255,365,740)) & mydata$c2.League==1) | (mydata$c1.PostCWLiberal==1 & mydata$c2.PostCWLiberal==0) | (mydata$c1.PostCWLiberal==0 & mydata$c2.PostCWLiberal==1)] <- 1

mydata$betweenorders <- 0
mydata$betweenorders[(mydata$c1.League==1 & mydata$ccode2 %in% c(255,365,740)) | (mydata$ccode1 %in% c(255,365,740) & mydata$c2.League==1) | (mydata$c1.PostWarLiberal==1 & mydata$c2.PostWarLiberal==0) | (mydata$c1.PostWarLiberal==0 & mydata$c2.PostWarLiberal==1) | (mydata$c1.PostWarCommunist==1 & mydata$c2.PostWarCommunist==0) | (mydata$c1.PostWarCommunist==0 & mydata$c2.PostWarCommunist==1)] <- 1

mydata$beyondorders <- 0
mydata$beyondorders[
  mydata$order == 0 &
    mydata$betweenorders == 0 &
    mydata$outsideorder == 0
] <- 1

### Data to adjust for political relevance ----

mydata |>
  ## add info for political relevance
  add_cow_majors() |>
  add_contiguity() |>
  add_capital_distance() -> mydata
logit <- function(x) 1 / (1 + exp(-x))
mydata |>
  mutate(
    contig = ifelse(
      conttype %in% 2:4,
      1, 0
    ),
    majdyad = pmax(cowmaj1, cowmaj2),
    
    ## Braumoeller Carson measure
    pol_rel = logit(
      4.801 + 4.50*contig - 1.051*log(capdist) + 2.901*majdyad
    )
  ) -> mydata

## Save dataset ----

write_rds(
  mydata,
  here::here("004_int_ord", "_data", "prep_data.rds")
)
