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

#options = webdriver.ChromeOptions()
#options.binary_location = "/Applications/Google Chrome 2.app/Contents/MacOS/Google Chrome"
#chrome_driver_binary = "/Users/apple/Documents/chromedriver"
#driver = webdriver.Chrome(chrome_driver_binary, chrome_options=options)
driver = webdriver.Chrome(r'C:\Users\13361\Downloads\chromedriver_win32\chromedriver.exe')


##Notes for future developers:
##
##Hi if you see this I am sorry that you have to read my horrible codes.
##This code is intended to collect addresses of contacts' info pages from the website
##HubSpot. This should be the first piece of code to run if you want to repeat the
##data collection process I have done.
##
##Finally, this is the work of Youming Liu as an
##undergraduate student at Washington University,
##if you have any difficulty to undertand my grosteque 
##coding, you may reach me via youmingliu@wustl.edu or
##youmingliu@aestas.cn as the last resort. I will help
##you out. This is the karma for my laziness today.


flist=[]


def hubspot_login():    
    driver.get ("https://app.hubspot.com/login")
    driver.find_element_by_id("username").send_keys("lyriewang@wustl.edu")
    driver.find_element_by_id ("password").send_keys("Washucelect2019")
    driver.find_element_by_id("loginBtn").click()

try:
    
    #login
    hubspot_login()
    securewait=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.ID, "hs-nav-v4")))   
    driver.get("https://app.hubspot.com/contacts/2767424/contacts/")

    ####### hover and click to have 100 items per page
    but=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//button[@aria-owns='dropdown-335']")))   

    if 'but' in vars():
        hover = ActionChains(driver).move_to_element(but)
        hover.perform()
        but.click()
        driver.find_element_by_xpath("//div[@id='dropdown-menu-334']/ul/li[3]").click()
     
    else:
        print('trying to find the dropdown menu element')
        
    expand=driver.find_element_by_xpath("//button[@aria-owns='dropdown-335']/span/span/i18n-string").text
    if 'expand' in vars() and expand=='100 per page':
        print('expanded the table to 100 items per page')
        for pagenum in range(94):
     #       for i in range(pagenum):
    #            driver.find_element_by_xpath("//button[@aria-label='Next page']").click()
    #        print("now is on the page "+str(pagenum))       
            clist=[]
            print('now is on the page' +str(pagenum+1))
            
            WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//table[@class='table private-table table-more-condensed table-condensed private-table--condensed private-table--flush table-hover private-table--hover']")))
            #WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, ".//a")))
            time.sleep(3)
            allhref=driver.find_elements_by_xpath('.//a')
            print('gathered all href links')
            

            for a in allhref:
                try:
                    clist.append(str(a.get_attribute('href')))
                except StaleException:
                    allhref=driver.find_elements_by_xpath('.//a')
                    clist.append(str(a.get_attribute('href')))
                    pass



            clist=[x for x in clist if x is not None]
            clist=[x for x in clist if "/contacts/2767424/contact/" in x]
            clist=list(set(clist))
            flist.append(clist)
            print('Successfully gathered info on this page')
            print("Number of items in the list is "+str(len(clist)))

           

            nextbut=WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH, "//button[@aria-label='Next page']")))
            #nextbut=driver.find_element_by_xpath("//button[@aria-label='Next page']")
            if 'nextbut' in vars():
                nextbut.click()               
            else:
                print("trying to find the next page button element")
                print('successfully hit the next page button')

    with open('fatherlist.txt','w') as f:
        f.write(str(flist))
        f.close()

            
                    
            
except Exception as e:

    print(e)
    exc_type, exc_obj, exc_tb = sys.exc_info()
    fname = os.path.split(exc_tb.tb_frame.f_code.co_filename)[1]
    print(exc_type, fname, exc_tb.tb_lineno)
    print('breaking at page'+str(pagenum))




