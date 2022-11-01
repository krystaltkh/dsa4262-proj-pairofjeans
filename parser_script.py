import json
import math
import pandas as pd
import os

file_path = input('Enter a file path: ')
output_file_path = input('Enter an output file path for the parsed csv file: ')

if os.path.exists(file_path):
    print('The file exists')
    jsondata = list()
    print("Started Reading JSON file.")
    with open(file_path, 'r') as f:
        # count = 0
        for jsonObj in f:
            # count += 1
            # print(count)
            jsonDict = json.loads(jsonObj)
            jsondata.append(jsonDict)

    print("Done reading JSON file.")

    data = list([[tr_id,pos,segment,reads] for row in jsondata \
                for tr_id,j in row.items() \
                for pos,y in j.items() \
                for segment,reads in y.items()])

    # counter = 0
    print("Summarising reads.")
    for i in data:
        n=len(i[3])
        # counter+=1
        # print(counter)
        for read in i[3]: #index 3 contains all reads
            zipped_reads = list(zip(*i[3]))
            mean_time1=sum(zipped_reads[0])/n
            mean_cur1=sum(zipped_reads[2])/n
            mean_time2=sum(zipped_reads[3])/n
            mean_cur2=sum(zipped_reads[5])/n
            mean_time3=sum(zipped_reads[6])/n
            mean_cur3=sum(zipped_reads[8])/n
            sum_sd1=0
            sum_sd2=0
            sum_sd3=0
            for read in i[3]:
                sum_sd1 += (read[1]**2 + (read[2] - mean_cur1)**2)
                sum_sd2 += (read[4]**2 + (read[5] - mean_cur2)**2)
                sum_sd3 += (read[7]**2 + (read[8] - mean_cur3)**2)
            combi_sd1 = math.sqrt(sum_sd1/n)
            combi_sd2 = math.sqrt(sum_sd2/n)
            combi_sd3 = math.sqrt(sum_sd3/n) 
        i.pop() # pop list of all reads in last index to replaced with summarised read
        i.extend([n,mean_time1,combi_sd1,mean_cur1,mean_time2,combi_sd2,mean_cur2,mean_time3,combi_sd3,mean_cur3])
    print("Done summarising reads.")

    data_final = pd.DataFrame(data, columns=['tr_id', 'pos', 'segment', 'num_reads',
                                'mean_time1', 'combined_sd1', 'mean_cur1',
                                'mean_time2', 'combined_sd2', 'mean_cur2',
                                'mean_time3', 'combined_sd3', 'mean_cur3'])

    data_final.to_csv(output_file_path, index=False, header=True)
    print("File saved to " + output_file_path)
else:
    print('The specified file does NOT exist')