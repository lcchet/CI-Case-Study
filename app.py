import streamlit as st
import pandas as pd
import plotly.express as px

st.set_page_config(page_title="Gen Re Case Study - CI Dashboard", layout="wide")
st.title("Meridian Life CI Portfolio Dashboard (2000–2025)")

# load and cache data
@st.cache_data
def load_data():
    policy = pd.read_csv("Policy_Data_Cleaned.csv")
    claims = pd.read_csv("Claims_Data_Final.csv")
    
    # calculate exposure years
    policy['Policy Issue Date'] = pd.to_datetime(policy['Policy Issue Date'])
    policy['Status Date'] = pd.to_datetime(policy['Status Date'])
    policy['Exposure_Years'] = (policy['Status Date'] - policy['Policy Issue Date']).dt.days / 365.25
    policy = policy[policy['Exposure_Years'] > 0]
    
    return policy, claims

policy_df, claims_df = load_data()

# get overall rates for the main chart
exposure_summary = policy_df.groupby('Product Name')['Exposure_Years'].sum().reset_index()
claims_summary = claims_df.groupby('Product Name').size().reset_index(name='Claim_Count')
overall_rates = pd.merge(exposure_summary, claims_summary, on='Product Name', how='left')
overall_rates['Claim_Count'] = overall_rates['Claim_Count'].fillna(0)
overall_rates['Incidence_Rate'] = (overall_rates['Claim_Count'] / overall_rates['Exposure_Years']) * 1000

fig_overall_rate = px.bar(
    overall_rates, 
    x='Product Name', 
    y='Incidence_Rate', 
    text_auto='.2f', 
    color='Product Name',
    title="Overall Incidence Rate Comparison",
    labels={'Incidence_Rate': 'Rate (per 1,000 Person-Years)', 'Product Name': ''}
)
fig_overall_rate.update_layout(showlegend=False, height=350)

# setup tabs
tab1, tab2 = st.tabs(["VitalCare Critical Illness Plan (VC20)", "LifeSecure Critical Illness Rider (LS30)"])

products = [
    "VitalCare Critical Illness Plan", 
    "LifeSecure Critical Illness Rider"
]
tabs = [tab1, tab2]

# render content for each product
for tab, product in zip(tabs, products):
    with tab:
        p_df = policy_df[policy_df['Product Name'] == product]
        c_df = claims_df[claims_df['Product Name'] == product]
        
        # calc kpis
        tot_exposure = p_df['Exposure_Years'].sum()
        tot_claims = len(c_df)
        inc_rate = (tot_claims / tot_exposure) * 1000 if tot_exposure > 0 else 0
        
        st.subheader(f"Key Metrics: {product}")
        kpi1, kpi2, kpi3 = st.columns(3)
        kpi1.metric("Total Valid Claims", f"{tot_claims}")
        kpi2.metric("Total Exposure (Years)", f"{tot_exposure:,.0f}")
        kpi3.metric("Product Incidence Rate", f"{inc_rate:.2f} per 1k")
        
        st.divider()
        
        # show the overall rate chart
        st.plotly_chart(fig_overall_rate, use_container_width=True, key=f"overall_rate_{product}")
        
        st.divider()
        
        # detailed charts and filters
        colA, colB = st.columns(2)
        
        with colA:
            st.subheader("Distribution of Claim Causes")
            all_causes = sorted(c_df['Claim Cause / Condition'].unique())
            
            selected_causes = st.multiselect(
                f"Filter conditions for {product}:", 
                options=all_causes, 
                default=all_causes,
                key=f"cause_filter_{product}" 
            )
            
            filtered_c_df = c_df[c_df['Claim Cause / Condition'].isin(selected_causes)]
            
            if not filtered_c_df.empty:
                cause_counts = filtered_c_df['Claim Cause / Condition'].value_counts().reset_index()
                cause_counts.columns = ['Condition', 'Count']
                
                fig_cause = px.bar(
                    cause_counts, 
                    x='Count', 
                    y='Condition', 
                    orientation='h',
                    text_auto=True,
                    color='Count',
                    color_continuous_scale='Blues'
                )
                fig_cause.update_layout(yaxis={'categoryorder':'total ascending'}, showlegend=False)
                st.plotly_chart(fig_cause, use_container_width=True, key=f"cause_chart_{product}")
            else:
                st.info("No claims match the selected conditions.")
                
        with colB:
            st.subheader("Age at Claim Distribution")
            if not filtered_c_df.empty:
                fig_age = px.histogram(
                    filtered_c_df, 
                    x='Age at Claim', 
                    nbins=15,
                    color_discrete_sequence=['#E69F00'],
                    labels={'Age at Claim': 'Age at Diagnosis'},
                    title="Frequency of Claims by Age"
                )
                fig_age.update_layout(bargap=0.1)
                st.plotly_chart(fig_age, use_container_width=True, key=f"age_chart_{product}")
            else:
                st.info("No data available.")