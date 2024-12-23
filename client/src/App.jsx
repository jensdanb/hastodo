import {useQuery, useMutation, useQueryClient, QueryClient, QueryClientProvider,} from '@tanstack/react-query'

import { NavBar } from "./components";
import { TodoApp } from "./components";
import { startDB } from './services/db-service';

const queryClient = new QueryClient()
startDB();

function App() {
  return (
    <QueryClientProvider client={queryClient}>
      <>
        <NavBar />
        <TodoApp
          initialFilter={"All"}/>
      </>
    </QueryClientProvider>
  )
}

export default App;