import json
import csv
from urllib.request import urlopen
from bs4 import BeautifulSoup

# Affiche une donnée JSON bien formatée
def printJSON(data):
	print(json.dumps(data, indent=2))


# Convertit une list de données JSON en CSV et les écrit dans un fichier
# Les éléments JSON de la liste doivent tous avoir les mêmes clés !
def writeCSVFileFromJSON(list_json, filename):
	f = csv.writer(open(filename, 'w'))
	
	headers = [key for key in list_json[0].keys()]
	f.writerow(headers)

	for element in list_json:
		f.writerow(list(element.values()))


# Extrait le code HTML d'une page web à partir de son URL
def getPageContentFromURL(url):
	if not url.startswith('http://'):
		url = 'http://en.espn.co.uk' + url

	response = urlopen(url)
	return response.read()


# Parse la page principale avec la liste des adversaires
# Retourne le contenu du tableau [0] et la liste des URL des listes de matches [1]
def parseMainPage(url):
	html = getPageContentFromURL(url)
	soup = BeautifulSoup(html, "html.parser")

	table = soup.find_all('table', {'class': 'engineTable'})

	matches_lists_urls = []
	opponents_list = []

	for table_row in table[2].find_all('tr', {'class': 'data1'}):
		columns = table_row.findAll('td')
		matches = {}

		if len(columns) <= 1:
			continue

		if columns[0].text == 'home':
			break

		link = columns[-1].find('a')

		# Ajoute une page à parser ensuite
		if link != None:
			matches_lists_urls.append(link['href'])

		# Récupère les données
		opponent_name = columns[0].text.replace('v ', '').replace('in ', '')
		if len(opponent_name) <= 2:
			continue

		opponents_list.append({
			'where': 'home' if 'v ' in columns[0].text else 'away',
			'opponent': opponent_name,
			'yearStart': columns[1].text.split('-')[0],
			'yearEnd': columns[1].text.split('-')[1],
			'matches': columns[2].text,
			'won': columns[3].text,
			'lost': columns[4].text,
			'draw': columns[5].text,
			'pointsFor': columns[7].text,
			'pointsAgainst': columns[8].text
		})

	return [opponents_list, matches_lists_urls]


# Parse une liste de pages qui contiennent les infos des matches contre un adversaire
# Retourne le contenu la liste des matches [0] et la liste des URL des détails des matches [1]
def parseMatchesLists(matches_lists_urls):
	matches_details_urls = []
	matches_list = []

	for url in matches_lists_urls:
		html = getPageContentFromURL(url)
		soup = BeautifulSoup(html, "html.parser")

		table = soup.find_all('table', {'class': 'engineTable'})


		for table_row in table[2].find_all('tr', {'class': 'data1'}):
			columns = table_row.findAll('td')
			matches = {}

			if len(columns) <= 1:
				continue

			link = columns[-1].find('a')

			# Ajoute une page à parser ensuite
			if link != None:
				matches_details_urls.append(link['href'])

			# Récupère les données
			opponent_name = columns[7].text.replace('v ', '').replace('in ', '')
			if len(opponent_name) <= 2:
				continue

			matches_list.append({
				'result': columns[0].text,
				'where': 'home' if 'v ' in columns[7].text else 'away',
				'opponent': opponent_name,
				'pointsFor': columns[1].text,
				'pointsAgainst': columns[2].text,
				'pointsForHalfTime': columns[4].text,
				'pointsAgainstHalfTime': columns[5].text,
				'date': columns[9].text
			})

	return [matches_list, matches_details_urls]


# http://en.espn.co.uk/statsguru/rugby/team/9.html?class=1;opposition=10;template=results;type=team;view=results


# Parsing de la page principale
MAIN_PAGE_URL = 'http://en.espn.co.uk/statsguru/rugby/team/9.html?class=1;template=results;type=team'

[opponents_list, matches_lists_urls] = parseMainPage(MAIN_PAGE_URL)

writeCSVFileFromJSON(opponents_list, 'opponents_list.csv')


# Parsing des liste des matches de chaque adversaire
[matches_list, matches_details_urls] = parseMatchesLists(matches_lists_urls)

writeCSVFileFromJSON(matches_list, 'matches_list.csv')