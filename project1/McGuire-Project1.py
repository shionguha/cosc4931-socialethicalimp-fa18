import csv
import numpy

averageData = numpy.zeros((31, 5)) 
# id, views, likes, dislikes, comments, numvid(avg col 5)

with open('USvideos.csv', 'rb') as csvfile:
	readCSV = csv.reader(csvfile, delimiter='	')
	categoryId = []
	views = []
	likes = []
	dislikes = []
	commentCount = []
	for row in readCSV:
		categoryId.append(row[0])
		views.append(row[1])
		likes.append(row[2])
		dislikes.append(row[3])
		commentCount.append(row[4])
			
		for x in categoryId[row]:
			if x == 10:
				categoryId[row] = 3
			if x == 15:
				categoryId[row] = 4
			if x > 16:
				categoryId[row] = categoryID[row] - 12
			
		for q in range(1, 4):
			# views
			averageData[categotyId[row[q], 5]] = averageData[categotyId[row[q], 5]] + 1
			averageData[categoryId[row[q]], q] = averageData[categoryId[row[q]], q] * averageData[categotyId[row[q]]] - 1) / numberofVideos) + averageData[categoryId[row[q]], q] / numberofVideos
			

		

numpy.savetxt("LiamData.csv", averageData, delimiter=",")
