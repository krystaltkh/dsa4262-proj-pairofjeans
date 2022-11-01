import json
import math
import pandas as pd
import os

json_path = input('Enter JSON file path (.json): ')
n_read_path = input('Enter n_reads file path (.readcount): ')
index_path = input('Enter index file path (.index): ')
output_file_path = input('Enter a file path for the parsed csv file (.csv): ')

if not os.path.exists(json_path):
    print("JSON file not found.")
if not os.path.exists(n_read_path):
    print("Data.readcount file not found.")
if not os.path.exists(index_path):
    print("Data.index file not found.")
if output_file_path[-3:] != "csv":
    output_file_path = output_file_path + ".csv"

elif os.path.exists(json_path) and os.path.exists(n_read_path) and os.path.exists(index_path):
    print('Files exists')
    jsondata = list()
    print("Started Reading JSON file.")
    with open(json_path, 'r') as f:
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

    data_length = len(data)

    counter = 1
    print("Summarising reads.")
    for i in data:
        # counter used to track progress
        if (counter == 1):
            print("Started row 1.")
        if counter == 0.25 * data_length or counter == 0.25*data_length+1:
            print("25% completed.")
        if counter == 0.5 * data_length or counter == 0.5*data_length+1:
            print("50% completed.")
        if counter == 0.75 * data_length+1 or counter == 0.75*data_length+1:
            print("75% completed.")
        n=len(i[3])
        counter+=1
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
        i.extend([mean_time1,combi_sd1,mean_cur1,mean_time2,combi_sd2,mean_cur2,mean_time3,combi_sd3,mean_cur3])
    print("Done summarising reads.")

    data_final = pd.DataFrame(data, columns=['tr_id', 'pos', 'segment', 
                                'mean_time1', 'combined_sd1', 'mean_cur1',
                                'mean_time2', 'combined_sd2', 'mean_cur2',
                                'mean_time3', 'combined_sd3', 'mean_cur3'])
    
    print("Adding read counts.")
    read_counts = pd.read_csv(n_read_path)
    data_final['n_reads'] = read_counts['n_reads']
    print("Done")
    print("Adding transcript lengths.")
    tr_len = pd.read_csv(index_path)
    tr_len['tr_length'] = tr_len['end'] - tr_len['start']
    data_final['tr_length'] = tr_len['tr_length']
    print("Done.")
    print("Writing to output file.")
    data_final.to_csv(output_file_path, index=False, header=True)
    print("Done. Parsed file found at " + output_file_path)
