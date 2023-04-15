from flask import Flask,render_template,request,jsonify,url_for
import pickle
import numpy as np
from requests import post
import json

#load the venues
with open('venues.json','r') as f:
    venues = json.load(f)    
#load the encoded teams
with open('encodedteams.json','r') as f:
    teams = json.load(f)    
#load the scaler
with open('scaler.pickle','rb') as f:
    scaler = pickle.load(f)       
#Load the model 
filename = 'model-Random.pkl'
regressor = pickle.load(open(filename,'rb'))

app = Flask(__name__)



@app.route('/')
def home():
    return render_template('index.html',venue  = venues)


@app.route('/example/<lower_limit>/<upper_limit>')
def example(lower_limit,upper_limit):
    return render_template("result.html", lower_limit=lower_limit,upper_limit=upper_limit)


@app.route('/predict',methods=['POST'])

def predict():
    temp_arr = [0,0]


    
    batting_team = request.json['batting']
    for i in teams:
        if i =='batting_team':
            ind = teams[i]
            temp_arr[0] = ind
            break

    bowling_team = request.json['bowling']
    for i in teams:
        if i =='bowling_team':
            ind = teams[i]
            temp_arr[1] = ind
            break
    
    
    venue = request.json["venues"]
    
    temp_ven = [0]*29
    for i in venues:
        if i =='venue':
            ind = venues[i]
            temp_ven.insert(ind,1)
            break


    

        


    overs = float(request.json['over'])
    runs = int(request.json['run'])
    wickets = int(request.json['wicket'])
    runs_in_prev_5 = int(request.json['run_last5'])
    wickets_in_prev_5 = int(request.json['wicket_last5']) 


    temp_arr =  [overs,wickets,runs,wickets_in_prev_5,runs_in_prev_5]+temp_arr+temp_ven
    
    

    data = np.array([temp_arr][0])
    data = scaler.transform([data])
    my_prediction = int(regressor.predict(data)[0])
    
    

    return jsonify({'redirect': url_for("example", lower_limit=my_prediction-10,upper_limit = my_prediction+5)})

        


if __name__ == '__main__':
    app.run(debug=True,port=8000)
