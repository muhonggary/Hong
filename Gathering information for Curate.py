import requests
import sys, os
import time
from selenium import webdriver
from selenium.webdriver.common.action_chains import ActionChains
from ast import literal_eval
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import StaleElementReferenceException as StaleException
import xlwt
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import TimeoutException


##Notes for future developers:
##
##Hi if you see this I am sorry that you have to read my horrible codes.
##This code is intended to collect information of contacts from the website
##HubSpot. But first you may want to run another piece of code to collect
##addresses of contacts' info pages which will be saved to a txt file named
##"fatherlist.txt". I should have named it as masterlist, for avoiding any
##BDSM implication (what am I saying).
##
##Finally, this is the work of Youming Liu as an
##undergraduate student at Washington University,
##if you have any difficulty to undertand my grosteque 
##coding, you may reach me via youmingliu@wustl.edu or
##youmingliu@aestas.cn as the last resort. I will help
##you out. This is the karma for my laziness today.


with open('fatherlist.txt','r') as f:
    source=f.read()
    f.close()
    
olist=literal_eval(source)

wb = xlwt.Workbook()
ws = wb.add_sheet("Sheet 1", cell_overwrite_ok=True)
##with open('safespace.txt','r',encoding='utf-8') as f:
##    source2=f.read()
##    f.close()
##    
###variables

##pncname=literal_eval(source2)
#pncname=[]
ppl=0
flist=[]
for l in olist:
    for a in l:
        flist.append(a)

#options = webdriver.ChromeOptions()
#options.binary_location = "/Applications/Google Chrome 2.app/Contents/MacOS/Google Chrome"
#chrome_driver_binary = "/Users/apple/Documents/chromedriver"
#driver = webdriver.Chrome(chrome_driver_binary, chrome_options=options)
driver = webdriver.Chrome(r'C:\Users\13361\Downloads\chromedriver_win32\chromedriver.exe')

ws.write(0,0,'want')
ws.write(0,1,'need')
ws.write(0,2,'afford')
ws.write(0,3,'number of unique events')
ws.write(0,4,'number of conversion events')
ws.write(0,5,'state')
ws.write(0,6,'country')
ws.write(0,7,'date of conversion')
ws.write(0,8,'rentals')
ws.write(0,9,'proposals')
ws.write(0,10,'COGS')
ws.write(0,11,'channel name')
ws.write(0,12,'campaign name')
ws.write(0,13,'original source')
ws.write(0,14,'website sessions')
ws.write(0,15,'website pages')
ws.write(0,16,'last referrer site')
ws.write(0,17,'first referrer site')
ws.write(0,18,'average website pages viewed')
ws.write(0,19,'sales email opened')
ws.write(0,20,'sales email delivered')
ws.write(0,21,'sales email clicked')
ws.write(0,22,'total revenue')
ws.write(0,23,'source to thank')
ws.write(0,24,'last sales email clicked')
ws.write(0,25,'last deal closed')
ws.write(0,26,'last deal amount')
ws.write(0,27,'average events per year')
ws.write(0,28,'number of sales activities')
ws.write(0,29,'likelihood to close')
ws.write(0,30,'life cycle stage')
ws.write(0,31,'date of last contact')
ws.write(0,32,'event professional type')
ws.write(0,33,'sales cycle')
ws.write(0,34,'date became opportunity')
ws.write(0,35,'date became customer')
ws.write(0,36,'number of deals')
ws.write(0,37,'average event revenue')
ws.write(0,38,'persona')
ws.write(0,39,'company')
ws.write(0,40,'contact')
ws.write(0,41,'number of employees')

#wb.save('test8.xls')
def check_exists_by_xpath(xpath):
    try:
        driver.find_element_by_xpath(xpath)
    except NoSuchElementException:
        return False
    return True  

def hubspot_login():    
    driver.get ("https://app.hubspot.com/login")
    driver.find_element_by_id("username").send_keys("lyriewang@wustl.edu")
    driver.find_element_by_id ("password").send_keys("Washucelect2019")
    driver.find_element_by_id("loginBtn").click()

try:
    
    #login

    hubspot_login()
    time.sleep(1)
    driver.get("https://app.hubspot.com/contacts/2767424/contacts/")
    for i in range(2263+379+1089+750+937+1275+826,len(flist)):

        driver.get (flist[i]+'properties?eschref=%2Fcontacts%2F2767424%2Fcontact%2F11426401%2F%3Finteraction%3Dnote&esclabel=Back%20to%20contact%20record')
        time.sleep(1)
        
##        if driver.find_elements_by_xpath("//h2[@class='private-heading-5 private-alert__title']")[0].text == 'There was a problem displaying this page.':
##            driver.refresh()
    
##        =WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='']")))
##        =WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='']")))
##        =WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='']")))
##        =WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='']")))
        try:
            want=WebDriverWait(driver, 3).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-qualifier_want']")))
        except TimeoutException:
            if check_exists_by_xpath("//h2[@class='private-heading-5 private-alert__title']")==True:
                driver.refresh()
                want=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-qualifier_want']")))
            
        wantvalue=want.get_attribute('value')
        need=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-qualifier_need']")))
        needvalue=need.get_attribute('value')
        afford=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-qualifier_afford']")))
        affordvalue=afford.get_attribute('value')
        numuniqevents=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-num_unique_conversion_events']")))
        numuniqeventsvalue=numuniqevents.get_attribute('value')
        numconversionevents=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-num_conversion_events']")))
        numconversionevents=numconversionevents.get_attribute('value')
        state=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-ip_state']")))
        statevalue=state.get_attribute('value')
        country=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-ip_country']")))
        countryvalue=country.get_attribute('value')
        dateconversion=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-first_conversion_date']")))
        dateconversionvalue=dateconversion.get_attribute('value')
        rentals=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-curate_rentals']")))
        rentalsvalue=rentals.get_attribute('value')
        proposal=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-curate_proposals']")))
        proposalvalue=proposal.get_attribute('value')
        cogs=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-curate_cogs']")))
        cogsvalue=cogs.get_attribute('value')
        channelname=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-channel_name']")))
        channelnamevalue=channelname.get_attribute('value')
        campaignname=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-campaign_name']")))
        campaignnamevalue=campaignname.get_attribute('value')
        orisource=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-hs_analytics_source']")))
        orisourcevalue=orisource.get_attribute('value')
        sessions=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-hs_analytics_num_visits']")))
        sessionsvalue=sessions.get_attribute('value')
        pages=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-hs_analytics_num_page_views']")))
        pagesvalue=pages.get_attribute('value')
        lastsite=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-hs_analytics_last_referrer']")))
        lastsitevalue=lastsite.get_attribute('value')
        firstsite=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-hs_analytics_first_referrer']")))
        firstsitevalue=firstsite.get_attribute('value')
        avgpage=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-hs_analytics_average_page_views']")))
        avgpagevalue=avgpage.get_attribute('value')
        eopen=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-hs_email_open']")))
        eopenvalue=eopen.get_attribute('value')
        edeli=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-hs_email_delivered']")))
        edelivalue=edeli.get_attribute('value')
        eclicked=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-hs_email_click']")))
        eclickedvalue=eclicked.get_attribute('value')
        totalrev=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-total_revenue']")))
        totalrevvalue=totalrev.get_attribute('value')
        sourcetothank=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-source_who_can_we_thank_']")))
        sourcetothankvalue=sourcetothank.get_attribute('value')
        lsalesclick=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-hs_sales_email_last_clicked']")))
        lsalesclickvalue=lsalesclick.get_attribute('value')
        lsalesclose=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-recent_deal_close_date']")))
        lsalesclosevalue=lsalesclose.get_attribute('value')
        lsalesnum=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-recent_deal_amount']")))
        lsalesnumvalue=lsalesnum.get_attribute('value')
        nofevent=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-average_events']")))
        nofeventvalue=nofevent.get_attribute('value')
        nofsales=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-num_notes']")))
        nofsalesvalue=nofsales.get_attribute('value')
        ltoclose=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-hs_predictivecontactscore_v2']")))
        ltoclosevalue=ltoclose.get_attribute('value')
        lc=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-lifecyclestage']")))
        lcvalue=lc.get_attribute('value')
        datelcont=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-notes_last_contacted']")))
        datelcontvalue=datelcont.get_attribute('value')
        eventtype=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-event_professional_type']")))
        eventtypevalue=eventtype.get_attribute('value')
        sc=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-days_to_close']")))
        scvalue=sc.get_attribute('value')
        dateopp=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-hs_lifecyclestage_opportunity_date']")))
        dateoppvalue=dateopp.get_attribute('value')
        datecust=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-hs_lifecyclestage_customer_date']")))
        datecustvalue=datecust.get_attribute('value')
        deals=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-num_associated_deals']")))
        dealsvalue=deals.get_attribute('value')
        aer=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-average_event_revenue']")))
        aervalue=aer.get_attribute('value')
        persona=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-hs_persona']")))
        pvalue=persona.get_attribute('value')
        company=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-company']")))
        comvalue=company.get_attribute('value')
        if comvalue=='':
            
            company=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//div[@data-unit-test='highlightSubtitle']")))
            comvalue=company.text


        email=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//div[@data-profile-property='email']/div/div/span/span")))
        emailtext=email.text.split('@')
        if emailtext!= ['']:
            if emailtext[1] in ['gmail.com','foxmail.com','yahoo.com','hotmail.com','aol.com']:
                emailvalue=emailtext[0]
            else:
                emailvalue=emailtext[1]
        else:
            emailvalue=''
        name=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.CLASS_NAME, "private-truncated-string__inner")))
        namevalue=name.text
        employee=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-number_employees']")))
        employeevalue=employee.get_attribute('value')
        lt=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-hs_analytics_source']")))
        ltvalue=lt.get_attribute('value')
        detail1=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-hs_analytics_source_data_1']")))
        detail1value=detail1.get_attribute('value')
        detail2=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//input[@data-selenium-test='property-input-hs_analytics_source_data_2']")))
        detail2value=detail2.get_attribute('value')

        

        
        if 'pvalue' in vars():
                print('gathered the persona value')
        else:
                print('element not found')

        if 'comvalue' in vars():
                print('gathered the company value')
        else:
                print('element not found')
        if 'namevalue' in vars():
                print('gathered the name value')                   
        else:
                print('element not found')

              

        
##        pncname.append([namevalue,comvalue,pvalue,employeevalue,ltvalue,detail1value,detail2value,emailvalue])
        #print(pncname)
        ppl+=1
        print("now finished the person "+str(ppl))

        ws.write(i+1,0,wantvalue)
        ws.write(i+1,1,needvalue)
        ws.write(i+1,2,affordvalue)
        ws.write(i+1,3,numuniqeventsvalue)
        ws.write(i+1,4,numconversionevents)
        ws.write(i+1,5,statevalue)
        ws.write(i+1,6,countryvalue)
        ws.write(i+1,7,dateconversionvalue)
        ws.write(i+1,8,rentalsvalue)
        ws.write(i+1,9,proposalvalue)
        ws.write(i+1,10,cogsvalue)
        ws.write(i+1,11,channelnamevalue)
        ws.write(i+1,12,campaignnamevalue)
        ws.write(i+1,13,orisourcevalue)
        ws.write(i+1,14,sessionsvalue)
        ws.write(i+1,15,pagesvalue)
        ws.write(i+1,16,lastsitevalue)
        ws.write(i+1,17,firstsitevalue)
        ws.write(i+1,18,avgpagevalue)
        ws.write(i+1,19,eopenvalue)
        ws.write(i+1,20,edelivalue)
        ws.write(i+1,21,eclickedvalue)
        ws.write(i+1,22,totalrevvalue)
        ws.write(i+1,23,sourcetothankvalue)
        ws.write(i+1,24,lsalesclickvalue)
        ws.write(i+1,25,lsalesclosevalue)
        ws.write(i+1,26,lsalesnumvalue)
        ws.write(i+1,27,nofeventvalue)
        ws.write(i+1,28,nofsalesvalue)
        ws.write(i+1,29,ltoclosevalue)
        ws.write(i+1,30,lcvalue)
        ws.write(i+1,31,datelcontvalue)
        ws.write(i+1,32,eventtypevalue)
        ws.write(i+1,33,scvalue)
        ws.write(i+1,34,dateoppvalue)
        ws.write(i+1,35,datecustvalue)
        ws.write(i+1,36,dealsvalue)
        ws.write(i+1,37,aervalue)
        ws.write(i+1,38,pvalue)
        ws.write(i+1,39,comvalue)
        ws.write(i+1,40,namevalue)
        ws.write(i+1,41,employeevalue)
    
    

##    with open('personav5.txt','w',encoding='utf-8') as f:
##        f.write(str(pncname))
##        f.close()


                
            
except Exception as e:

    print(e)
    exc_type, exc_obj, exc_tb = sys.exc_info()
##    fname = os.path.split(exc_tb.tb_frame.f_code.co_filename)[1]
##    print(exc_type, fname, exc_tb.tb_lineno)
##    print('breaking at '+str(ppl))
##    with open('safespace.txt','w',encoding='utf-8') as f:
##        f.write(str(pncname))
##        f.close()
##    print('Stored the data into the safe space')

wb.save('test11.xls')



