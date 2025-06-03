# How to bootstrap the project

## Bootstrap project using the Feliz template
```
dotnet new feliz -n TimeCalculator -o .
```

### Explanations

- `-n TimeCalculator` : name of the project
- `-o .` : output to the current directory

## Upgrade vite packages

```
  "devDependencies": {
    "@vitejs/plugin-react": "^4.2.1",
    "vite": "^5.1.4"
  },
```


## Install Tailwind CSS

```
npm install tailwindcss @tailwindcss/vite
```

## Update the package.json

Add the following line in package.json to enable ES modules, which is required for Vite and modern JavaScript tooling:

```
"type": "module",
```





