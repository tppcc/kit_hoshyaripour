import bz2
import os
from urllib.parse import urljoin

import requests
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.firefox import GeckoDriverManager
from selenium.webdriver.firefox.options import Options


def HttpsScan(url):
    """
    Function to scan the directory of the input url using Beautiful Soup

    :param url: The URL of the directory to be scanned
    :type url: str
    :return: list of urls, file names to the target file if successfully accessed, otherwise an empty list
    :rtype: List[str]
    """

    # Setup Firefox options for headless mode
    options = Options()
    #options.headless = True
    options.add_argument('--headless')

    # Initialize the Firefox driver
    driver = webdriver.Firefox(service=Service(GeckoDriverManager().install()), options=options)
    driver.implicitly_wait(10)

    # Open the URL
    driver.get(url)

    # Wait for the page to load completely
    try:
        WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.ID, "teleports")))

        # Find all links in the directory listing
        links = driver.find_elements(By.TAG_NAME, "a")

        # Extract the file names from the links
        urls = [link.get_attribute('href') for link in links if link.get_attribute('href') and not link.get_attribute('href').endswith('/')]
        files = [os.path.basename(url) for url in urls]
        return urls, files

    finally:
        # Close the browser
        driver.quit()

    '''
    try:
        response = requests.get(url)
        if response.status_code == 200:
            soup = BeautifulSoup(response.content, 'html.parser')
            links = soup.find_all('a', href=True)
            urls = [urljoin(url, link['href']) for link in links]
            files = [link['href'] for link in links]
            return urls, files
        else:
            print(f"Failed to scan directory: {response.status_code}")
            return []
    except Exception as e:
        print(f"Error: {e}")
        return []
    '''

def RequestWebFile(file_url, fname, directory):
    r"""
    Function to request the target file from remote server

    :param file_url: The URL of the file to be requested.
    :type file_url: str
    :param fname: The filename to save the downloaded file as.
    :type fname: str
    :param directory: Local target directory of the downloaded file
    :type directory: str
    :return: True if the file was successfully downloaded and extracted, False otherwise.
    :rtype: bool
    """

    # Data consistency check
    assert type(file_url) == type(fname) == type(directory) == str, "Input should be string"

    # Define the header setup for downloading the files
    headers = {'X-Client-Token': 'Qg9Z4I0WWinLKQVYjVQw2MKLYrmXVXh5-0DiVEzl6rA'}

    target_path = os.path.join(directory, fname)

    if os.path.exists(target_path) == False:
        response = requests.get(file_url, headers=headers)
        if response.status_code == 200:
            with open(target_path, 'wb') as f:
                f.write(response.content)

            # Check if the downloaded file is a grib2 file
            if target_path.endswith('.grib2'):
                try:
                    ## Open the downloaded file and decompress it
                    #with open(target_path, 'rb') as f_bz2:
                    #    decompressed_content = bz2.decompress(f_bz2.read())
                    ## Write the decompressed content back to the file
                    #with open(target_path[:-4], 'wb') as f_decompressed:
                    #    f_decompressed.write(decompressed_content)
                    ## Remove the bz2 file
                    #os.remove(target_path)
                    return True
                except Exception as e:
                    print(f"Error decompressing file: {e}")
                    return False
            else:
                return True
        else:
            return False
    else:
        return True


def ModelLoad(model, parameters_dict):
    r"""Entry point of the wget routine of the
    :param model:
    :return:
    """

    env_dict = parameters_dict

    vnames = [x for x in env_dict.__getattribute__((model + '_variable'))]
    for vname in vnames:
        fdir = env_dict.local_directory[model]
        # Create local direcotry if not exists
        if not os.path.exists(fdir):
            os.makedirs(fdir)
        local_storage_path = os.path.join(fdir, vname)
        # Create local direcotry if not exists
        if not os.path.exists(local_storage_path):
            os.makedirs(local_storage_path)
        url = urljoin(env_dict.model_url[model],
                      env_dict.__getattribute__((model + '_variable'))[vname])
        fileurls, filenames = HttpsScan(url)
        print(fileurls)
        # Remove ../ path from scaned list
        fileurls = fileurls[3:]
        filenames = filenames[3:]
        for i in range(len(fileurls)):
            status = RequestWebFile(fileurls[i], filenames[i], local_storage_path)
            assert status == True, "Web request failed"
